;; -*- lexical-binding: t; -*-

(defun neutron--get-title ()
  "Extract the #+title: value from the current buffer."
  (cadr (assoc "TITLE" (org-collect-keywords '("TITLE")))))

(defun neutron--get-summary ()
  "Extract text content under the * summary heading, or nil if absent/empty."
  (let* ((tree (org-element-parse-buffer))
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

(defun neutron--get-id ()
  "Extract the :ID: property from the :PROPERTIES: drawer, or nil if absent."
  (let ((tree (org-element-parse-buffer)))
    ;; The document node is the root; get its :ID: property.
    (org-element-property :ID tree)))

(defun neutron--ensure-heading (file-path heading &optional parent)
  "Ensure HEADING exists in FILE-PATH.
FILE-PATH is the target file.
HEADING is the heading name to ensure.
PARENT, if given, inserts HEADING as a subheading under PARENT."
  (let ((buf (find-file-noselect file-path)))
    (with-current-buffer buf
      (save-excursion
        (unless (org-find-exact-headline-in-buffer heading)
          (if parent
              (progn
                (goto-char (org-find-exact-headline-in-buffer parent))
                (forward-line)
                (insert "** " heading "\n"))
            (goto-char (point-min))
            (if (re-search-forward "^\\*" nil t)
                (beginning-of-line)
              (goto-char (point-max)))
            (insert "* " heading "\n")))
        (save-buffer)))))

(defun neutron--ensure-index-structure (file-path)
  "Ensure both * index and ** project headings exist in FILE-PATH.
FILE-PATH is the target file."
  (neutron--ensure-heading file-path "index")
  (neutron--ensure-heading file-path "project" "index"))

(defun neutron--find-heading-node (heading)
  "Find and return the AST heading node, or error if not found.
HEADING is the heading name to search for."
  (let ((tree (org-element-parse-buffer)))
    (or (org-element-map tree 'headline
          (lambda (hl)
            (when (string= (org-element-property :raw-value hl) heading)
              hl))
          nil t)
        (error "Heading \"%s\" not found" heading))))

(defun neutron--find-item-by-id (items id)
  "Search ITEMS for one with matching org-roam ID. Return item or nil.
ITEMS is a list of org-element item nodes.
ID is the org-roam ID to match."
  (let ((id-pattern (format "\\[\\[id:%s\\]" (regexp-quote id))))
    (seq-find (lambda (item)
                (let ((item-text (buffer-substring
                                  (org-element-property :contents-begin item)
                                  (org-element-property :contents-end item))))
                  (string-match id-pattern item-text)))
              items)))

(defun neutron--build-link-content (id title old-summary summary locked)
  "Build link content string.
ID is the org-roam ID.
TITLE is the link display text.
OLD-SUMMARY is the existing summary to preserve if locked.
SUMMARY is the new summary.
LOCKED, if non-nil, preserves OLD-SUMMARY and adds ! to the link."
  (let ((content (format "[[id:%s][%s]]: %s"
                         id title (or old-summary summary ""))))
    (if locked (replace-regexp-in-string "\\]\\]:" "]]!:" content) content)))

(defun neutron--extract-old-summary (item-text)
  "Extract summary from ITEM-TEXT if locked (has !), else return nil.
ITEM-TEXT is the raw text of the list item."
  (when (string-match "\\]\\]!: " item-text)
    (substring item-text (match-end 0))))

(defun neutron--update-item (item new-content)
  "Replace ITEM's content with NEW-CONTENT.
ITEM is an org-element item node.
NEW-CONTENT is the replacement text."
  (let ((item-begin (org-element-property :contents-begin item))
        (item-end (org-element-property :contents-end item)))
    (delete-region item-begin item-end)
    (insert new-content)))

(defun neutron--insert-new-item (insert-pos id title summary)
  "Insert a new list item at INSERT-POS with an org-roam link.
INSERT-POS is the buffer position to insert at.
ID is the org-roam ID.
TITLE is the link display text.
SUMMARY is the description after the link."
  (goto-char insert-pos)
  (if (org-in-item-p)
      (org-insert-item)
    (insert "- "))
  (insert (format "[[id:%s][%s]]: %s\n" id title (or summary ""))))

(defun neutron--upsert-index-link (file-path heading id title &optional summary insert)
  "Insert or update a link under HEADING in FILE-PATH by org-roam ID.
The format of the link index item is: - [[id:UUID][Title]]: Summary.

FILE-PATH is the target file.
HEADING is the subheading of 'index' to insert under.
ID is the org-roam ID to match and use.
TITLE is the link display text.
SUMMARY is the description after the link.
INSERT non-nil prepends; nil appends (default).

If the existing link has ! (e.g., [[id:UUID][Title]]!: Summary...),
the summary is locked and won't be replaced."
  (let ((buf (find-file-noselect file-path)))
    (with-current-buffer buf
      (save-excursion
        (let* ((heading-node (neutron--find-heading-node heading))
               (contents (org-element-contents heading-node))
               (items (org-element-map contents 'item (lambda (item) item)))
               (found-item (neutron--find-item-by-id items id)))
          (if found-item
              ;; Update.
              (let* ((item-text (buffer-substring
                                 (org-element-property :contents-begin found-item)
                                 (org-element-property :contents-end found-item)))
                     (locked (string-match "\\]\\]!:" item-text))
                     (old-summary (neutron--extract-old-summary item-text))
                     (new-content (neutron--build-link-content id title old-summary summary locked)))
                (neutron--update-item found-item new-content))
            ;; Insert.
            (let ((insert-pos (or (if insert
                                      (org-element-property :contents-begin heading-node)
                                    (org-element-property :contents-end heading-node))
                                  ;; Empty heading: position after heading line.
                                  (save-excursion
                                    (goto-char (org-element-property :begin heading-node))
                                    (forward-line)
                                    (point)))))
              (neutron--insert-new-item insert-pos id title summary)))))
      (save-buffer))))

(provide 'neutron-org)
