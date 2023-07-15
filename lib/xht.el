(require 'ht)

(defun xht-derive (htable from to mutator &optional delete)
  "From the HTABLE change the value FROM (key) TO (key) the
return value of MUTATOR. You can optionally DELETE FROM
afterwards. For example:
  #s('one 1) => (xht-derive ht 'one 'two (lambda (v) (+ v 1)) t) => #s('two 2)"
  (ht-set htable to (funcall mutator (ht-get htable from)))
  (when delete (ht-remove htable from)))

(defun xht-pluck (htable key)
  "Get value from KEY from HTABLE, and remove KEY
afterwards."
  (let* ((v (ht-get htable key)))
    (ht-remove htable key)
    v))

(defun xht-default (key default ht)
  "If the value for KEY doesn't exist in HT,
set it to DEFAULT.

If DEFAULT is a function, it's called without args;
otherwise DEFAULT is a value."
  (unless (gethash key ht) ; only if it doesn't have a value
    (puthash key
             (if (functionp default)
                 (funcall default)
               default)
             ht)))

(defun xht-setp (key htable)
  "Return t if KEY points to a non-nil value in HTABLE."
  (let* ((r (ht-get htable key)))
    (not (eq nil r))))

(defun xeq (a b)
  "A WIP function that takes any two values and compares
them for equality."
  (cond ((and (stringp a) (stringp b)) (string= a b))
        ((and (eq nil  a) (eq nil  b)) t)
        ((and (eq t    a) (eq t    b)) t)
        ((and (numberp a) (numberp b)) (= a b))
        nil))

(defun xht= (key value htable)
  (xeq (ht-get htable key) value))
