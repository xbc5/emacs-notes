(require 'org-drill)

(setq xdrill-root (xroam-path "flashcards"))

(defun xdrill--pick ()
  "Pick a flashcard file."
  (f-pick (directory-files xdrill-root nil "\\.org$")
          :root xdrill-root
          :prompt "Choose a flashcard"))

(defun xdrill-run ()
  "Pick a set of flashcards."
  (interactive)
  (org-drill (list (xdrill--pick))))

(defun xdrill-add ()
  "Find an existing flashcard file or create one."
  (interactive)
  (mkdir xdrill-root t)
  (switch-to-buffer
   (set-buffer
    (org-capture-target-buffer
     (xorg-file-name-fix
      (xdrill--pick)))))
  (point-max))

(defun xdrill-question-add ()
  "Insert a new question on the next line."
  (interactive)
  (org-insert-heading nil)
  (insert "flashcard :drill:")
  (newline-and-indent))

(defun xdrill-answer-add ()
  "Insert a new answer on the next line."
  (interactive)
  (org-insert-subheading nil)
  (insert "answer")
  (newline-and-indent))
