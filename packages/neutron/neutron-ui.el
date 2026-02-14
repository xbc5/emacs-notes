;; -*- lexical-binding: t; -*-

(defun neutron--prompt (message)
  "Prompt the user for input with MESSAGE."
  (read-string message))

(provide 'neutron-ui)
;;; neutron-ui.el ends here.
