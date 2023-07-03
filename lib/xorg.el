(defun xorg-props (vals)
  "Turn VALS into a single string suitable for Org properties:
- surround with \"\";
- remove excess spaces;

VALS can be a string or a list of strings. If it's a single
string, you can sparate values with ';', and it will split
on that.
  e.g. \"  foo ;  bar   x ;baz\" => '\"foo\" \"bar x\" \"baz\"'
  e.g. (\"  foo  \" \"bar  x  \" \"baz  \") => '\"foo\" \"bar x\" \"baz\"'"
  (let ((seq nil))
    (cond ((cl-typep vals 'string)
           (setq seq (s-split ";" vals)))
          ((cl-typep vals 'list)
           (setq seq vals))
          (t (error (format "VALS must be a string or sequence, not '%s'" (type-of vals)))))
    (s-join " " (mapcar (lambda (s) (format "\"%s\"" (xstr-neat s))) seq))))
