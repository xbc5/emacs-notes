;; -*- lexical-binding: t; -*-
(require 'neutron-constants)
(require 'neutron-fs)
(require 'f)
(require 'seq)

(defun neutron--get-title ()
  "Extract the #+title: value from the current buffer."
  (cadr (assoc "TITLE" (org-collect-keywords '("TITLE")))))

(defun neutron--set-title (title &optional file-path)
  "Replace #+title: with TITLE in FILE-PATH.
TITLE is the new title.
FILE-PATH is optional and defaults to the current buffer's file."
  (let ((buf (find-file-noselect (or file-path (buffer-file-name)))))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        ;; Find and replace the title keyword (case-insensitive match).
        (let ((case-fold-search t))
          (when (re-search-forward "^#\\+title:.*$" nil t)
            (replace-match (format "#+title: %s" title))))))))

(defun neutron--set-org-properties (properties)
  "Set each property in PROPERTIES on the heading at point.
PROPERTIES is an alist of (KEY . VALUE) pairs."
  (dolist (prop properties)
    (org-set-property (car prop) (cdr prop))))

(defun neutron--get-summary (&optional ast)
  "Extract text under the * summary heading. Return nil if absent or empty.
AST is an optional pre-parsed org-element tree."
  (let* ((tree (or ast (org-element-parse-buffer)))
         ;; Find the summary heading in the parse tree.
         (summary (org-element-map tree 'headline
                    (lambda (hl)
                      (when (string= (org-element-property :raw-value hl) "summary")
                        hl))
                    nil t)))
    ;; Extract and export the heading's contents as plain text.
    (when summary
      (let ((text (string-trim
                   (org-export-string-as
                    (org-element-interpret-data
                     (org-element-contents summary))
                    'ascii t))))
        (when (not (string-empty-p text))
          text)))))

(defun neutron--get-id (&optional ast)
  "Extract the :ID: property from the :PROPERTIES: drawer, or nil if absent.
AST is an optional pre-parsed org-element tree."
  (let ((tree (or ast (org-element-parse-buffer))))
    ;; The document node is the root; get its :ID: property.
    (org-element-property :ID tree)))

(defun neutron--get-id-from-file-path (path)
  "Extract the org-roam ID from the file at PATH.
PATH is the file path to read."
  (with-current-buffer (find-file-noselect path)
    (neutron--get-id)))

(defun neutron--get-props-from-file-path (path)
  "Extract index-related properties from the file at PATH.
PATH is the file path to read.
Returns (:title TITLE :summary SUMMARY :id ID)."
  (with-current-buffer (find-file-noselect path)
    (neutron--get-index-related-props)))

(defun neutron--get-index-related-props (&optional ast)
  "Return a plist of index-related properties from the current buffer.
AST is an optional pre-parsed org-element tree.
Returns (:title TITLE :summary SUMMARY :id ID)."
  (let ((tree (or ast (org-element-parse-buffer))))
    (list :title (neutron--get-title)
          :summary (neutron--get-summary tree)
          :id (neutron--get-id tree))))

(defun neutron--ensure-heading (file-path heading &optional parent)
  "Ensure HEADING exists in FILE-PATH.
FILE-PATH is the target file.
HEADING is the heading name to ensure.
PARENT, if given, inserts HEADING as a subheading under PARENT."
  (let ((buf (find-file-noselect file-path)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (error "neutron--ensure-heading only works in Org buffers"))
      (save-excursion
        ;; Skip if heading already exists.
        (unless (org-find-exact-headline-in-buffer heading)
          (if parent
              ;; If given a parent, append a subheading to avoid
              ;; clobbering existing headings.
              (progn
                (goto-char (org-find-exact-headline-in-buffer parent))
                ;; Derive the child level from the parent so headings nest correctly.
                (let ((child-stars (make-string (1+ (org-outline-level)) ?*)))
                  (org-end-of-subtree t)
                  (insert "\n" child-stars " " heading "\n")))
            ;; Otherwise, insert a top-level heading.
            (goto-char (point-min))
            (if (re-search-forward "^\\*" nil t)
                ;; An existing heading was found, so insert before it.
                (beginning-of-line)
              (goto-char (point-max)))
            ;; Insert the new top-level heading.
            (insert "\n* " heading "\n")))))))

(defun neutron--ensure-index-structure (file-path)
  "Ensure both * index and ** project headings exist in FILE-PATH.
FILE-PATH is the target file."
  (neutron--ensure-heading file-path "index")
  (neutron--ensure-heading file-path "project" "index"))

(defun neutron--find-heading-in-ast (ast heading)
  "Find a headline node by name in AST.
AST is a parsed org-element tree or subtree.
HEADING is the heading name to find."
  (org-element-map ast 'headline
    (lambda (hl)
      (when (string= (org-element-property :raw-value hl) heading)
        hl))
    nil t))

(defun neutron--find-items-by-id (ast id)
  "Find all list items in AST containing a link to org-roam ID.
AST is a parsed org-element tree or subtree.
ID is the org-roam ID to match."
  (let (result)
    (org-element-map ast 'link
      (lambda (link)
        ;; Match id-type links with the target ID.
        (when (and (string= (org-element-property :type link) "id")
                   (string= (org-element-property :path link) id))
          ;; Walk up to the enclosing item node.
          (let ((parent (org-element-property :parent link)))
            (while (and parent (not (eq (org-element-type parent) 'item)))
              (setq parent (org-element-property :parent parent)))
            (when parent
              (push parent result))))))
    (nreverse result)))

(defun neutron--build-item (id title &optional summary locked)
  "Build an item AST node with an org-roam link.
ID is the org-roam ID.
TITLE is the link display text.
SUMMARY is the optional description after the link.
LOCKED, if non-nil, adds ! to preserve the summary."
  (let* ((lock-char (if locked "!" ""))
         ;; Only append ": summary" when a summary exists.
         (suffix (if summary (format "%s: %s" lock-char summary) ""))
         (text (format "[[id:%s][%s]]%s" id title suffix)))
    (org-element-create 'item '(:bullet "- " :pre-blank 0)
                        (org-element-create 'paragraph nil text))))

(defun neutron--extract-old-summary (item-text)
  "Extract summary from ITEM-TEXT if locked (has !), else return nil.
ITEM-TEXT is the interpreted text of the list item."
  (when (string-match "\\]\\]!:[ ]*" item-text)
    (string-trim (substring item-text (match-end 0)))))

(defun neutron--update-item (old-item new-item)
  "Replace OLD-ITEM with NEW-ITEM in the AST.
OLD-ITEM is the existing item node in the parse tree.
NEW-ITEM is the replacement item node."
  (org-element-set-element old-item new-item))

(defun neutron--insert-new-item (heading-node item &optional prepend)
  "Add ITEM to the plain-list under HEADING-NODE.
HEADING-NODE is the target heading AST node.
ITEM is the item AST node to insert.
PREPEND, if non-nil, inserts at the beginning of the list."
  (let ((plain-list (org-element-map heading-node 'plain-list #'identity nil t)))
    (unless plain-list
      ;; No list exists, so create one and attach it to the heading.
      (setq plain-list (org-element-create 'plain-list '(:type unordered)))
      (org-element-adopt-elements heading-node plain-list))
    ;; Insert item into the list.
    (if (and prepend (org-element-contents plain-list))
        (org-element-insert-before item (car (org-element-contents plain-list)))
      (org-element-adopt-elements plain-list item))))

(defun neutron--upsert-index-link (file-path heading id title &optional summary prepend)
  "Insert or update a link under HEADING in FILE-PATH by org-roam ID.
The format of the link index item is: - [[id:UUID][Title]]: Summary.

FILE-PATH is the target file.
HEADING is the subheading of `'index to insert under.
ID is the org-roam ID to match and use.
TITLE is the link display text.
SUMMARY is the description after the link.
PREPEND, if non-nil, inserts at the beginning of the list.

If the existing link has ! (e.g., [[id:UUID][Title]]!: Summary...),
the summary is locked and won't be replaced."
  (let ((buf (find-file-noselect file-path)))
    (with-current-buffer buf
      (save-excursion
        ;; Ensure the document contains an index heading before starting.
        (neutron--ensure-index-structure file-path)

        ;; Create the AST.
        (let* ((ast (org-element-parse-buffer))
               (index-node (neutron--find-heading-in-ast ast "index")))
          ;; Process the index node if it exists.
          (when index-node
            ;; Search for existing links with the same ID.
            (let ((found-items (neutron--find-items-by-id index-node id)))
              (if found-items
                  ;; Update all matching links in the AST.
                  (dolist (item found-items)
                    (let* ((item-text (org-element-interpret-data item))
                           (locked (string-match "\\]\\]!:" item-text)) ;; Detect locked items (marked with !).
                           (old-summary (neutron--extract-old-summary item-text)))
                      ;; Change the existing item…
                      (neutron--update-item
                       ;; …by replacing it.
                       item (neutron--build-item id title
                                                 (if locked old-summary summary) locked))))
                ;; No existing link found, so insert a new one under the specified heading.
                (let ((heading-node (neutron--find-heading-in-ast index-node heading)))
                  ;; If index heading is missing, create it and re-parse.
                  (unless heading-node
                    (neutron--ensure-heading file-path heading "project")
                    (setq ast (org-element-parse-buffer))
                    (setq index-node (neutron--find-heading-in-ast ast "index"))
                    (setq heading-node (neutron--find-heading-in-ast index-node heading)))
                  ;; Create the link.
                  (neutron--insert-new-item heading-node
                                            (neutron--build-item id title summary)
                                            prepend)))
              ;; Write back the index subtree.
              (let ((beg (org-element-property :begin index-node))
                    (end (org-element-property :end index-node)))
                (delete-region beg end)
                (goto-char beg)
                (insert (org-element-interpret-data index-node))))))))))

(defun neutron--remove-index-link (file-path id &optional ast)
  "Remove links by org-roam ID from index.project in FILE-PATH, at any depth.
Ignores index.project.related.
FILE-PATH is the target index file.
ID is the org-roam ID to remove.
AST is an optional pre-parsed org-element tree."
  (let ((buf (find-file-noselect file-path)))
    (with-current-buffer buf
      (save-excursion
        (let* ((tree (or ast (org-element-parse-buffer)))
               (index-node (neutron--find-heading-in-ast tree "index"))
               (project-node (and index-node (neutron--find-heading-in-ast index-node "project")))
               (related-node (and project-node (neutron--find-heading-in-ast project-node "related"))))
          (when project-node
            ;; Find items with the target ID under index.project.
            (let ((found-items (neutron--find-items-by-id project-node id)))
              ;; Exclude items under "related", because those are manually managed.
              (when related-node
                (let ((related-items (neutron--find-items-by-id related-node id)))
                  (setq found-items
                        (seq-remove (lambda (item) (memq item related-items))
                                    found-items))))
              ;; In the AST, bullet-list items are wrapped in an invisible plain-list node:
              ;;
              ;;   * index
              ;;   ** project
              ;;   *** children
              ;;       - [[id:abc][Child A]]: summary
              ;;       - [[id:def][Child B]]: summary
              ;;   *** siblings
              ;;       - [[id:ghi][Sibling]]: summary
              ;;
              ;;   headline "children"
              ;;     └── plain-list        <-- :parent returns this
              ;;         ├── item "Child A"
              ;;         └── item "Child B"
              ;;
              ;; If we want to remove Child A, we get the plain-list that
              ;; wraps it, then filter Child A out of that plain-list's contents.
              (dolist (item found-items)
                (let* ((bullet-list (org-element-property :parent item))
                       (remaining (seq-remove (lambda (child) (eq child item))
                                              (org-element-contents bullet-list))))
                  (if remaining
                      (org-element-set-contents bullet-list remaining)
                    ;; Empty plain-lists crash org-element-interpret-data,
                    ;; so remove the entire plain-list from its containing section.
                    ;;
                    ;; Get the section node that contains the bullet-list (one level up).
                    (let ((section (org-element-property :parent bullet-list)))
                      ;; Replace the section's children with everything except the empty bullet-list.
                      (org-element-set-contents
                       section
                       (seq-remove (lambda (child) (eq child bullet-list))
                                   (org-element-contents section))))))))
            ;; Write back the index subtree.
            (when index-node
              (let ((beg (org-element-property :begin index-node))
                    (end (org-element-property :end index-node)))
                (delete-region beg end)
                (goto-char beg)
                (insert (org-element-interpret-data index-node))))))))))

(defun neutron--upsert-bidirectional-links (anchor neighbour neighbour-heading anchor-heading id title summary)
  "Upsert bidirectional links between ANCHOR and NEIGHBOUR, then save NEIGHBOUR.
ANCHOR is the anchor file.
NEIGHBOUR is the neighbour file.
NEIGHBOUR-HEADING is the heading in NEIGHBOUR to insert the anchor link under.
ANCHOR-HEADING is the heading in ANCHOR to insert the neighbour link under.
ID, TITLE, SUMMARY are the anchor's properties."
  ;; Track open state before opening, so we can close it afterward if we opened it.
  (let ((was-open (find-buffer-visiting neighbour))
        (neighbour-props (with-current-buffer (find-file-noselect neighbour)
                           (neutron--get-index-related-props))))
    (neutron--upsert-index-link neighbour neighbour-heading id title summary)
    (neutron--upsert-index-link anchor anchor-heading
                                (plist-get neighbour-props :id)
                                (plist-get neighbour-props :title)
                                (plist-get neighbour-props :summary))
    (let ((buf (find-file-noselect neighbour)))
      (with-current-buffer buf (save-buffer))
      (unless was-open (kill-buffer buf)))))

(defun neutron--sync-index-links (&optional file-path)
  "Synchronize index links for FILE-PATH based on its type.
If FILE-PATH is an index, add it to parent index and add all children to it.
If FILE-PATH is a sibling, add it to local index.
FILE-PATH defaults to the buffer file name."
  (let* ((file (or file-path (buffer-file-name)))
         (props (neutron--get-index-related-props))
         (title (plist-get props :title))
         (summary (plist-get props :summary))
         (id (plist-get props :id))
         (file-type (neutron--file-type file)))
    ;; Guard against re-entry from saves triggered on neighbour files.
    (let ((neutron--syncing t))
      (pcase file-type
        ;; If the current file is an index.org file.
        ('index
         (when-let ((parent-index (neutron--get-parent-index file)))
           (neutron--upsert-bidirectional-links file parent-index "children" "parent" id title summary))
         (dolist (child-index (neutron--get-child-indexes file))
           (neutron--upsert-bidirectional-links file child-index "parent" "children" id title summary))
         (dolist (sibling (neutron--get-siblings file))
           (neutron--upsert-bidirectional-links file sibling "home" "siblings" id title summary)))
        ('sibling
         (when-let ((local-index (neutron--get-local-index file)))
           (neutron--upsert-bidirectional-links file local-index "siblings" "home" id title summary)))
        (type
         (error "Unknown file type: %s" type))))
    nil))

(defun neutron--delete-two-way-parent-link (node-file-path node-id parent-file-path node-ast)
  "Delete a bidirectional link between NODE-FILE-PATH and its parent.
NODE-FILE-PATH is the target node's index file path.
NODE-ID is the node's org-roam ID.
PARENT-FILE-PATH is the parent index file path.
NODE-AST is a pre-parsed org-element tree for the node's index file."
  ;; Parse parent once, so both calls reuse the same AST.
  (let* ((parent-buf (find-file-noselect parent-file-path))
         (parent-ast (with-current-buffer parent-buf
                       (org-element-parse-buffer)))
         (parent-props (with-current-buffer parent-buf
                         (neutron--get-index-related-props parent-ast))))
    ;; Remove the node from the parent's list.
    (neutron--remove-index-link parent-file-path node-id parent-ast)
    ;; Remove the parent link from the node.
    (neutron--remove-index-link node-file-path (plist-get parent-props :id) node-ast)))

(defun neutron-set-project-status (&optional file-path)
  "Set project status via selection.
FILE-PATH is optional and defaults to the current buffer."
  (interactive)
  (let ((file (or file-path (buffer-file-name)))
        (sorted-statuses (sort (copy-sequence neutron-project-statuses) #'string<)))
    (with-current-buffer (find-file-noselect file)
      (let ((status (completing-read "Status: " sorted-statuses nil t)))
        ;; Apply to file-level :PROPERTIES: drawer, not the closest heading.
        (org-entry-put (point-min) "NEUTRON_PROJECT_STATUS" status)
        ;; We want to save buffer here because the user calls this directly.
        (save-buffer)
        nil))))

(defun neutron--find-project ()
  "Select a neutron project, returning its index.org path.
Pre-selects the current project or last selected project."
  ;; Get only directories with an index.org, since those are actual projects.
  (let* ((project-dirs (neutron--get-dirs neutron-dir))
         ;; Display relative paths, so the user sees "foo/bar" not full paths.
         (choices (mapcar (lambda (d) (f-relative d neutron-dir)) project-dirs))
         ;; The user may be inside a project already, so pre-select it.
         ;; Fall back to the last selection for repeated task creation.
         (preselected-file (or neutron--last-selected-project-index
                               (ignore-errors (neutron--get-project-index))))
         (preselected (when preselected-file
                        (f-relative (f-dirname preselected-file) neutron-dir)))
         (choice (completing-read "Project: " choices nil t preselected)))
    (when choice
      (let ((file (f-join neutron-dir choice "index.org")))
        ;; Remember the selection, so repeated calls don't re-prompt from scratch.
        (setq neutron--last-selected-project-index file)
        file))))

(defun neutron--disconnect-node (&optional path ast)
  "Disconnect the node at PATH from the graph.
PATH must be a file or directory under `neutron-dir'. If PATH is a
directory, it must contain an index.org file. If not provided, PATH
defaults to the directory of the current file.
AST is an optional pre-parsed org-element tree for the node's index file.
Return t on success, nil on error."
  ;; AST without a path is unusable, because we need the path for remove-index-link.
  (when (and ast (not path))
    (error "neutron--disconnect-node: AST provided without a path"))
  ;; Normalize path: if it's a file, get its directory; if it's a directory, use it as-is.
  (let* ((default-dir (file-name-directory (buffer-file-name)))
         (input-dir (or path default-dir))
         (normalized-dir (if (file-directory-p input-dir) input-dir
                           (file-name-directory input-dir)))
         (index-file-path (expand-file-name "index.org" normalized-dir)))
    ;; The directory must be a node (has an index.org within neutron-dir).
    (if (not (and (file-in-directory-p normalized-dir neutron-dir)
                  (file-exists-p index-file-path)))
        (progn
          (message (concat "neutron--disconnect-node: path is not a Neutron node: '%s'."
                           "Must have an index.org file within it.")
                   normalized-dir)
          nil)
      ;; Parse the node's index file once, so we can reuse the AST.
      (let* ((tree (or ast (with-current-buffer (find-file-noselect index-file-path)
                             (org-element-parse-buffer))))
             (id (neutron--get-id tree))
             (file-type (neutron--file-type index-file-path)))
        ;; Remove parent reference depending on node type.
        (pcase file-type
          ('index
           (when-let ((parent-index (neutron--get-parent-index index-file-path)))
             (neutron--delete-two-way-parent-link index-file-path id parent-index tree))
           t)
          ('sibling
           (when-let ((local-index (neutron--get-local-index index-file-path)))
             (neutron--delete-two-way-parent-link index-file-path id local-index tree))
           t)
          (type
           (message "neutron--disconnect-node: unknown file type: %s" type)
           nil))))))

(defun neutron--heading-path (hl tasks-node)
  "Return the ancestor heading names of HL up to TASKS-NODE.
HL is the headline node.
TASKS-NODE is the boundary node to stop at."
  (let ((node (org-element-property :parent hl))
        parts)
    ;; Walk up collecting ancestor names, stopping at tasks-node.
    (while (and node (not (eq node tasks-node)))
      (when (eq (org-element-type node) 'headline)
        (push (org-element-property :raw-value node) parts))
      (setq node (org-element-property :parent node)))
    (push (org-element-property :raw-value hl) parts)
    (string-join parts " > ")))

(defun neutron--subtree-has-todo-keyword-p (hl tasks-node)
  "Return non-nil if HL or any ancestor up to TASKS-NODE has a TODO keyword.
Use this in the AST to filter headings with TODO-related keywords.
HL is the headline node to check.
TASKS-NODE is the boundary node to stop at."
  (let ((node hl)
        (keywords '("TODO" "NEXT" "WAIT" "DONE" "DROP"))
        (found nil))
    (while (and node (not (eq node tasks-node)) (not found))
      (when (member (org-element-property :todo-keyword node) keywords)
        (setq found t))
      (setq node (org-element-property :parent node)))
    found))

(defun neutron--ensure-tasks-heading (file-path)
  "Ensure a * tasks heading exists in FILE-PATH, creating it if absent.
FILE-PATH is the target org file."
  (neutron--ensure-heading file-path "tasks"))

(defun neutron--get-task-targets ()
  "Return neutron refile targets.
Resolves to the first suitable tasks file specified by `neutron-task-files'.
Excludes headings with TODO keywords and their descendants."
  (let (targets)
    (dolist (dir (neutron--get-dirs neutron-dir))
      ;; Find the first available file in `neutron-task-files'. These are
      ;; files configured to store tasks.
      (when-let ((file (seq-find #'f-exists-p
                                 (mapcar (lambda (name)
                                           (f-join dir (concat name ".org")))
                                         neutron-task-files))))
        (with-current-buffer (find-file-noselect file)
          ;; These files should always have a "tasks" heading.
          (neutron--ensure-tasks-heading file)
          (let* ((ast (org-element-parse-buffer))
                 (tasks-node (neutron--find-heading-in-ast ast "tasks")))
            ;; Always include the tasks heading itself as a target.
            (push (list (neutron--format-path dir)
                        file nil
                        (org-element-property :begin tasks-node))
                  targets)
            ;; Include valid sub-headings under tasks.
            (org-element-map (org-element-contents tasks-node) 'headline
              (lambda (hl)
                (unless (neutron--subtree-has-todo-keyword-p hl tasks-node)
                  (push (list (concat (neutron--format-path dir) " > "
                                      (neutron--heading-path hl tasks-node))
                              file nil
                              (org-element-property :begin hl))
                        targets))))))))
    (nreverse targets)))

(defun neutron--validate-refile-region ()
  "Abort with an error if the active region violates org-refile's level constraint.
org-refile behaves unexpectedly if any heading in the region is above the first,
so validate upfront to avoid corrupting notes."
  (when (use-region-p)
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (let* ((ast (org-element-parse-buffer))
             (headings (org-element-map ast 'headline #'identity))
             (top-level (and headings (org-element-property :level (car headings)))))
        (when top-level
          (dolist (hl headings)
            (when (< (org-element-property :level hl) top-level)
              (user-error "First selected heading must be at the top level"))))))))

(provide 'neutron-org)
;;; neutron-org.el ends here.
