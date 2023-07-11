(defun xnum-len (int)
  "Get the length of a number, e.g. 123 => 3."
  (+ (floor (log int 10)) 1))
