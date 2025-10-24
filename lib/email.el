(defvar my/email--conf-dir-path
  (expand-file-name "~/.config/emacs")
  "The directory where offline email configuration lives.")

(defun my/email--make-conf-dir ()
  (make-directory my/email--conf-dir-path t))
