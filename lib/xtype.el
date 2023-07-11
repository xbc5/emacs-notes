(defun xnil-ish (var)
  "Return t if VAR (symbol) is unbound or nil."
  (or
   (not (boundp var))
   (eq nil (symbol-value var))))

(defun xtruthy (val)
  "Return t if VAL is not nil."
  (not (eq val nil)))
