(require 'f)

(defvar my/email--conf-dir-path
  (expand-file-name "~/.config/emacs-email")
  "The directory where offline email configuration lives.")

(defun my/email--make-conf-dir ()
  (make-directory my/email--conf-dir-path t))

(defun my/email--load-config-file ()
  (my/email--make-conf-dir) ; Must run before loading configs.
  (load-file (f-join my/email--conf-dir-path "conf.el")))
