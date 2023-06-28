(defun xstr-t (str)
  "Return t if STR is a non-blank (truthy) string."
  (and (stringp str) (not (string-blank-p str))))

(defun xstr-default (str default)
  "If STR is nil or blank, return default, otherwise STR."
  (if (xnil-or-blank str) default str))

(defun xnil-or-blank (str)
  "Return t if STR is a nil or a blank string."
  (or (eq nil str) (not (eq nil (string-blank-p str)))))
