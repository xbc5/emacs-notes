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
  "Ensure HEADING exists in FILE-PATH. If PARENT is given, insert as subheading under PARENT."
  (let ((buf (find-file-noselect file-path)))
    (with-current-buffer buf
      (save-excursion
        (unless (org-find-exact-headline-in-buffer heading)
          (if parent
              ;; Insert as subheading under parent.
              (progn
                (goto-char (org-find-exact-headline-in-buffer parent))
                (org-insert-subheading nil)
                (beginning-of-line)
                (delete-char -1)
                (end-of-line)
                (insert heading)
                (forward-line)
                (delete-blank-lines))
            ;; Insert as top-level heading before the first heading.
            (goto-char (point-min))
            (if (re-search-forward "^\\*" nil t)
                (beginning-of-line)
              (goto-char (point-max)))
            (insert (format "* %s\n" heading))))
        (save-buffer)))))

(defun neutron--ensure-index-structure (file-path)
  "Ensure both * index and ** project headings exist in FILE-PATH."
  (neutron--ensure-heading file-path "index")
  (neutron--ensure-heading file-path "project" "index"))

(provide 'neutron-org)
