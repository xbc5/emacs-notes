(defun xtime-decade (year)
  "Floor YEAR to the nearest decade:
  e.g. 1919 => 1910."
  (* (floor (/ year 10)) 10))
