(defun xstr-neat (str)
  "Trim and remove excess spaces from STR
(i.e. one space between words).

Return `STR.
"
  (replace-regexp-in-string " +" " " (s-trim str)))

(defun xstr-t (str)
  "Return t if STR is a non-blank (truthy) string."
  (and (stringp str) (not (string-blank-p str))))

(defun xstr-default (str default)
  "If STR is nil or blank, return default, otherwise STR."
  (if (xnil-or-blank str) default str))

(defun xnil-or-blank (str)
  "Return t if STR is a nil or a blank string."
  (or (eq nil str) (not (eq nil (string-blank-p str)))))

(defun xstr-truncate (str max-len)
  "When a STR is longer than MAX-LEN, remove the
extraneous characters.

Returns either a truncated string, or the original
STR."
  (if (length> str max-len)
      (substring str 0 max-len)
    str))

(defun xstr-split (delim str &optional downcase)
  "Split a string, making each item neat (trimmed
and excess spaces removed); remove nuls."
  (seq-filter (lambda (s) (not (string-blank-p s)))
              (mapcar (lambda (s) (if downcase
                                      (downcase (xstr-neat s))
                                    (xstr-neat s)))
                      (s-split delim str))))
