(defun xtime-decade (year)
  "Floor YEAR to the nearest decade:
  e.g. 1919 => 1910."
  (* (floor (/ year 10)) 10))

(defun xtime-current-year ()
  (nth 5 (parse-time-string (current-time-string))))

(defun xtime-year-p (year &optional pred)
  "Determine if YEAR is the future, current, or past.

PRED is a predicate that compares two numbers.
YEAR is a string or a number in the YYYY format.

Examples:
  (xtime-year-p 1999)    ; => nil  (default =)
  (xtime-year-p 1999 '<) ; => t
  (xtime-year-p 3999 '>) ; => t
"
  (let* ((yr (if (stringp year) (string-to-number year) year))
         (prd (or pred '=)))
    (funcall prd yr (xtime-current-year))))
