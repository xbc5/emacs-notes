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

(defun neutron--ensure-index-heading (file-path)
  "Ensure the * index heading exists in FILE-PATH, creating it if necessary at the top of headings."
  (let ((buf (find-file-noselect file-path)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (unless (org-find-exact-headline-in-buffer "index")
          ;; Insert before the first heading, or at end if no headings exist.
          (if (re-search-forward "^\\*" nil t)
              (beginning-of-line)
            (goto-char (point-max)))
          (insert "* index\n\n"))
        (save-buffer)))))

(provide 'neutron-org)
