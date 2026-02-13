;; -*- lexical-binding: t; -*-
(require 'neutron-fs)

(defun neutron--get-title ()
  "Extract the #+title: value from the current buffer."
  (cadr (assoc "TITLE" (org-collect-keywords '("TITLE")))))

(defun neutron--get-summary (&optional ast)
  "Extract text content under the * summary heading, or nil if absent/empty.
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
              ;; If given a parent, APPEND a subheading, so we
              ;; don't clobber existing headings.
              (progn
                (goto-char (org-find-exact-headline-in-buffer parent))
                ;; Derive the child level from the parent, so headings nest correctly.
                (let ((child-stars (make-string (1+ (org-outline-level)) ?*)))
                  (org-end-of-subtree t)
                  (insert "\n" child-stars " " heading "\n")))
            ;; Othrwise, insert a top-level heading.
            (goto-char (point-min))
            (if (re-search-forward "^\\*" nil t)
                ;; We found a random heading, so Insert before it.
                (beginning-of-line)
              (goto-char (point-max)))
            ;; No heading found, so insert one in the "empty" file.
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
      ;; No list exists — create one and adopt into the heading.
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
HEADING is the subheading of 'index' to insert under.
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

        ;; Create the AST
        (let* ((ast (org-element-parse-buffer))
               (index-node (neutron--find-heading-in-ast ast "index")))
          ;; Get and use an existing index node.
          (when index-node
            ;; Search for identical links.
            (let ((found-items (neutron--find-items-by-id index-node id)))
              (if found-items
                  ;; Update all matching links in the AST.
                  (dolist (item found-items)
                    (let* ((item-text (org-element-interpret-data item))
                           (locked (string-match "\\]\\]!:" item-text)) ;; [link]! is gets ignored.
                           (old-summary (neutron--extract-old-summary item-text)))
                      ;; Change the existing item…
                      (neutron--update-item
                       ;; …by replacing it.
                       item (neutron--build-item id title
                                                 (if locked old-summary summary) locked))))
                ;; Exiting links not found. Instead, insert new link under specified heading.
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
    (pcase file-type
      ;; If the current file is an index.org file.
      ('index
       ;; Create a two-way link relationship with the parent index.
       (when-let ((parent-index (neutron--get-parent-index file)))
         ;; Insert link into the parent.
         (neutron--upsert-index-link parent-index "children" id title summary)
         (let ((parent-props (with-current-buffer (find-file-noselect parent-index)
                               (neutron--get-index-related-props))))
           ;; Insert parent link into the current file.
           (neutron--upsert-index-link file "parent"
                                       (plist-get parent-props :id)
                                       (plist-get parent-props :title)
                                       (plist-get parent-props :summary))))
       ;; Create a two-way link relationship with child indexes.
       (dolist (child-index (neutron--get-child-indexes file))
         ;; Open the child and get its props.
         (let ((child-props (with-current-buffer (find-file-noselect child-index)
                              (neutron--get-index-related-props))))
           ;; Insert child links into the local index file.
           (neutron--upsert-index-link file "children"
                                       (plist-get child-props :id)
                                       (plist-get child-props :title)
                                       (plist-get child-props :summary))
           ;; Insert the local index link into the child.
           (neutron--upsert-index-link child-index "parent" id title summary)))
       ;; Create a two-way link relationship with siblings.
       (dolist (sibling (neutron--get-siblings file))
         (let ((sib-props (with-current-buffer (find-file-noselect sibling)
                            (neutron--get-index-related-props))))
           ;; Insert sibling link into this index.
           (neutron--upsert-index-link file "siblings"
                                       (plist-get sib-props :id)
                                       (plist-get sib-props :title)
                                       (plist-get sib-props :summary))
           ;; Insert this index link into the sibling.
           (neutron--upsert-index-link sibling "home" id title summary))))
      ;; If the current file is a sibling.
      ('sibling
       ;; Create a two-way link relationship with the local index.
       (when-let ((local-index (neutron--get-local-index file)))
         ;; Insert a backlink into the local index.
         (neutron--upsert-index-link local-index "siblings" id title summary)
         (let ((index-props (with-current-buffer (find-file-noselect local-index)
                              (neutron--get-index-related-props))))
           ;; Insert a local index link into this file.
           (neutron--upsert-index-link file "home"
                                       (plist-get index-props :id)
                                       (plist-get index-props :title)
                                       (plist-get index-props :summary)))))
      (type
       (error "Unknown file type: %s" type)))
    ;; Save at the end, so intermediate saves don't trigger hooks on incomplete state.
    (neutron--save-modified-buffers)))

(defun neutron--delete-index-links-related-to (&optional file-path)
  "Remove all index links to FILE-PATH from related files.
If FILE-PATH is an index, remove it from parent, children, and siblings.
If FILE-PATH is a sibling, remove it from the local index.
FILE-PATH defaults to the buffer file name."
  (let* ((file (or file-path (buffer-file-name)))
         (props (neutron--get-index-related-props))
         (id (plist-get props :id))
         (file-type (neutron--file-type file)))
    (pcase file-type
      ('index
       ;; Remove this index from parent's children list.
       (when-let ((parent-index (neutron--get-parent-index file)))
         (neutron--remove-index-link parent-index id))
       ;; Remove this index from each child's parent link.
       (dolist (child-index (neutron--get-child-indexes file))
         (neutron--remove-index-link child-index id))
       ;; Remove this index from each sibling's home link.
       (dolist (sibling (neutron--get-siblings file))
         (neutron--remove-index-link sibling id)))
      ('sibling
       ;; Remove this sibling from the local index.
       (when-let ((local-index (neutron--get-local-index file)))
         (neutron--remove-index-link local-index id)))
      (type
       (error "Unknown file type: %s" type)))
    ;; Save at the end, so intermediate saves don't trigger hooks on incomplete state.
    (neutron--save-modified-buffers)))

(provide 'neutron-org)
