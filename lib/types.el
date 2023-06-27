(defun xstr-t (str)
  "Return t if STR is a non-blank (truthy) string."
  (and (stringp str) (not (string-blank-p str))))
