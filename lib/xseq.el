(defun xhas (seq val)
  "Check if sequence SEQ has VAL.

Returns t or nil."
  (not (eq nil (member val seq))))

(defun xhas-any (seq vals)
  "Check if sequence SEQ has any VALS.

Returns t or nil."
  (length>
   (seq-filter (lambda (v)
                 (member v seq))
               vals)
   0))

(defun xhas-all (seq vals)
  "Check if sequence SEQ has all VALS.

Returns t or nil."
  (length=
   (seq-filter (lambda (v)
                 (member v seq))
               vals)
   (length vals)))

(defun xseq-sneat (seq &optional order-p)
  "Make a sequence (SEQ) of strings neat.
Trim each one, remove duplicates, and sort by ORDER-P"
  (seq-uniq
   (sort
    (seq-filter
     #'xstr-t
     (mapcar (lambda (v) (xstr-neat v)) seq))
    (or order-p #'string<))))

(defun xseq-listify (val)
  "If VAL is a list, return a list, else return a list.
Returns a list. Yes it does."
  (cond ((cl-typep val 'list) val)
        ((cl-typep val 'vector) (append val ()))
        (t (list val))))
