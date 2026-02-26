;; -*- lexical-binding: t; -*-

(defun neutron--prompt (message)
  "Prompt the user for input with MESSAGE."
  (read-string message))

(defun neutron--pick-schedule ()
  "Prompt for a repeat interval and return a scheduled date string for today.
If the entered value doesn't start with `+', one is prepended."
  (let* ((interval (completing-read "Interval: " '("+1d" "+2d" "+3d" "+1w" "+2w" "+1m" "+3m" "+6m" "+1y")))
         (interval (if (string-prefix-p "+" interval) interval (concat "+" interval)))
         (interval (if (string-match-p "\\+[0-9]+$" interval) (concat interval "d") interval)))
    (format-time-string (concat "<%Y-%m-%d %a " interval ">"))))

(provide 'neutron-ui)
;;; neutron-ui.el ends here.
