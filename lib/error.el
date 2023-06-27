(defun xcheck (msg &rest preds)
  "Check that all PREDS are t, otherwise error with
MSG. MSG is a string or a cons cell. If you pass a
string, it's used as the error message. If you pass
a cons cell (MSG . CTX), then CTX is used as additional
information in the error message like so: \"MSG: 'CTX'\"."
  (let* ((m (if (-cons-pair? msg) (car msg) msg))
         (ctx (when (-cons-pair? msg) (cdr msg))))

    ;; nil is a valid PRED, the caller might pass a PRED
    ;; as a msg by mistake.
    (unless (and (stringp m) (not (string-blank-p m)))
      (error "MSG must be a non-empty string"))

    (when (--any? (eq nil it) preds)
      (error (if ctx
                 (format "%s: '%s'" m ctx)
               msg)))))
