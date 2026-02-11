;; -*- lexical-binding: t; -*-

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
                (org-end-of-subtree t)
                (insert "\n** " heading "\n"))
            ;; Othrwise, insert a top-level heading.
            (goto-char (point-min))
            (if (re-search-forward "^\\*" nil t)
                ;; We found a random heading, so Insert before it.
                (beginning-of-line)
              (goto-char (point-max)))
            ;; No heading found, so insert one in the "empty" file.
            (insert "* " heading "\n")))
        (save-buffer)))))

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
         (text (format "[[id:%s][%s]]%s: %s" id title lock-char (or summary ""))))
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
                    (neutron--ensure-heading file-path heading "index")
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
                (insert (org-element-interpret-data index-node)))
              (save-buffer))))))))

(defun neutron--remove-index-link (file-path id)
  "Remove links by org-roam ID from the index in FILE-PATH.
FILE-PATH is the target index file.
ID is the org-roam ID to remove."
  (let ((buf (find-file-noselect file-path)))
    (with-current-buffer buf
      (save-excursion
        (let* ((ast (org-element-parse-buffer))
               (index-node (neutron--find-heading-in-ast ast "index")))
          (when index-node
            ;; Find all items with this ID.
            (let ((found-items (neutron--find-items-by-id index-node id)))
              ;; The same ID might appear in multiple headings, so find which lists contain our items and update all of them.
              (let ((parent-lists (delete-dups
                                   (mapcar (lambda (item)
                                             (org-element-property :parent item))
                                           found-items))))
                ;; Remove matched items from each parent list.
                (dolist (list-node parent-lists)
                  (org-element-set-contents
                   list-node
                   (seq-filter (lambda (child)
                                 (not (memq child found-items)))
                               (org-element-contents list-node))))))
            ;; Write back the index subtree.
            (let ((beg (org-element-property :begin index-node))
                  (end (org-element-property :end index-node)))
              (delete-region beg end)
              (goto-char beg)
              (insert (org-element-interpret-data index-node)))
            (save-buffer)))))))

(defun neutron--sync-index-links (&optional file-path)
  "Synchronize index links for FILE-PATH based on its type.
If FILE-PATH is an index, add it to parent index.
If FILE-PATH is a sibling, add it to local index.
FILE-PATH defaults to the buffer file name."
  (let* ((file (or file-path (buffer-file-name)))
         (title (neutron--get-title))
         (summary (neutron--get-summary))
         (id (neutron--get-id))
         (file-type (neutron--file-type file)))
    (pcase file-type
      ('index
       (when-let ((parent-index (neutron--get-parent-index file)))
         (neutron--upsert-index-link parent-index "project" id title summary))
       (dolist (child-index (neutron--get-child-indexes file))
         (neutron--sync-index-links child-index)))
      ('sibling
       (let ((local-index (f-join (f-dirname file) "index.org")))
         (neutron--upsert-index-link local-index "project" id title summary)))
      (type
       (error "Unknown file type: %s" type)))))

(provide 'neutron-org)
