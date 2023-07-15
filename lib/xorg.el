(defun xorg-stringify-prop (vals)
  "Turn VALS into a single string suitable for Org properties:
- surround with \"\";
- remove excess spaces;

VALS can be a string or a list of strings. If it's a single
string, you can sparate values with ';', and it will split
on that.
  e.g. \"  foo ;  bar   x ;baz\" => '\"foo\" \"bar x\" \"baz\"'
  e.g. (\"  foo  \" \"bar  x  \" \"baz  \") => '\"foo\" \"bar x\" \"baz\"'"
  (cond ((cl-typep vals 'string)
         (if (string-blank-p vals)
             ""
           (s-join " " (mapcar
                        (lambda (s) (format "\"%s\"" (xstr-neat s)))
                        (s-split ";" vals)))))
        ((eq vals nil) "")
        ((cl-typep vals 'seq)
         (s-join " "
                 (mapcar
                  (lambda (s) (format "\"%s\"" (xstr-neat s)))
                  vals)))
        ((cl-typep vals 'number)
         vals)
        (t (error (format "Cannot stringify type: '%s'" (type-of vals)))))
  )
