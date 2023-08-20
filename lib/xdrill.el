(require 'org-drill)

(defun xdrill-definitions ()
  "Get all flashcards for definitions."
  (interactive)
  (org-drill (directory-files (xroam-path-definitions) t "\\.org$")))
