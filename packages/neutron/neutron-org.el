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

(provide 'neutron-org)
