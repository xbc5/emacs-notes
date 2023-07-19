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

(defun xlist-insert-alpha (lst strs)
  "Insert a STRS into a LST of strings, alphabetically
(if it doesn't exist).

Returns the sorted LST."
  (seq-sort #'string<
            (cond ((stringp strs)
                   (add-to-list 'lst strs))
                  ((seqp strs) ; matches nil
                   (seq-uniq (append lst strs) #'xeq)) ; but essentially drops it
                  (t (error "Cannot append type '%s'" (type-of strs))))))

(defun xeq (a b)
  "A WIP function that takes any two values and compares
them for strict equality (type and value)."
  (cond ((and (stringp a) (stringp b)) (string= a b))
        ((and (eq nil  a) (eq nil  b)) t)
        ((and (eq t    a) (eq t    b)) t)
        ((and (symbolp a) (symbolp b)) (eq a b))
        ((and (numberp a) (numberp b)) (= a b))
        nil))
