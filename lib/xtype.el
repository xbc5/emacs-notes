(defun xnil-ish (var)
  "Return t if VAR (symbol) is unbound or nil."
  (or
   (not (boundp var))
   (eq nil (symbol-value var))))

(defun xtruthy (val)
  "Return t if VAL is not nil."
  (not (eq val nil)))

(defun xlist-like-p (val)
  "Return t if VAL is a list like sequence, and
not a string."
  (and (cl-typep val 'seq) (not (cl-typep val 'string))))
