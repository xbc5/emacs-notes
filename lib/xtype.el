(defun xnil-ish (var)
  "Return t if VAR (symbol) is unbound or nil."
  (or
   (not (boundp var))
   (eq nil (symbol-value var))))
