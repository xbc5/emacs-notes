(defun my/get-bibtex-key (node)
  "A BibTeX key picker using completing-read."
  (completing-read "Citation key: "
                   (mapcar #'(lambda (x) (cdr (assoc "=key=" x)))
                           (bibtex-completion-candidates))))
