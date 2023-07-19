(require 'ht)

(defun xht-mutate (htable cmds)
  "Mutate an HTABLE according to CMDS:
  (xht-mutate
    (ht ('one 1) ('two 2))
    '((rename one foo) (delete two)))

This example will return (ht ('foo 1)):
renaming one => foo; and deleting two.

It will run each command in order.

Rename and delete are the two supported
commands for now.

Returns the mutated HTABLE."
  (seq-doseq (c cmds)
    (let* ((cmd (car c))
           (key (nth 1 c))
           (arg (nth 2 c)))
      (when (ht-contains-p htable key)
        (cond ((eq cmd 'rename)
               (when (eq arg nil) (error "You must a new key name to rename '%s'" key))
               (xht-rename htable key arg))
              ((eq cmd 'delete) (ht-remove htable key))
              (t (error "Unknown command %s; cannot mutate hash table" cmd))))))
  htable)

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

(defun xht-rename (htable from to)
  "Move value in HTABLE FROM an old key TO a new key."
  (ht-set htable to (xht-pluck htable from)))

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

(defun xht= (key value htable)
  (xeq (ht-get htable key) value))

(defun xht-equal (t1 t2)
  "Compare two hash tables for deep equality.

Credit: https://stackoverflow.com/a/18196708
https://stackoverflow.com/help/licensing"
  (cond ((and (= (ht-size t1) 0) (= (ht-size t2) 0)) t)
        (t (and
            (= (ht-size t1) (ht-size t2))
            (--all? (eq it t) (ht-map (lambda (k v)
                                        (if (ht-p v)
                                            (xht-equal (ht-get t1 k) v)
                                          (equal (ht-get t1 k) v))
                                        ) t2))))))

(defun xht-keys (&rest htables)
  "For all HTABLES, return a set of keys."
  (let* ((result nil))
    (seq-doseq (htable htables)
      (maphash (lambda (k _) (add-to-list 'result k)) htable))
    result))


(defun xht-from-lists (items)
  "Like ht-from-alist, except it doesn't unwrap lists, e.g:
      NOT: '((\"k\" (1))) => ((\"k\" 1))
  INSTEAD: '((\"k\" (1))) => ((\"k\" (1)))
ht does this because (cons 1 '(1)) => (cons 1 1).

This will create a hash-table from a list of lists without
taking the initiative of doing whatever the fuck it wants.

ITEMS is a list of 2-lists '((k \"value\") (l (1 2)) ...),
where a 2-list is a list of N=2: (k v).

EXAMPLE:
  (xht-from-lists '((k \"v\") (j (1))))

Returns a hash table."
  (let* ((htable (ht)))
    (seq-doseq (i items)
      (ht-set htable (car i) (nth 1 i)))
    htable))
