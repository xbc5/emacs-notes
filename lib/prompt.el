(defun xprompt (prompt &optional required?)
  "Prompt with PROMPT. If REQUIRED then error on
blank string."
  (let* ((txt (s-trim (read-string (concat prompt ": ")))))
    (when required?
      (xcheck "You MUST provide a value" (not (string-blank-p txt))))
    txt))

(defun xprompt-url (prompt &optional required?)
  "Prompt for a URL and validate it. If REQUIRED then
error on blank string."
  (let ((url (xprompt prompt required?))
        (err "You MUST provide a secure URL, not"))
    (unless (string-blank-p url)
      (xcheck (cons err url) (xurl-https? url)))
    url))

(defun xprompt-aliases ()
  "Prompt for multiple aliases, separated by ';'.
Don't worry about leading or trailing spaces."
  (s-join " " (mapcar (lambda (s)
                        (format "\"%s\"" (s-trim s)))
                      (s-split ";" (read-string "Alias: ") t))))

(defun xprompt-rating (&optional msg)
  "Prompt the user for a rating between 0-100. Return nil
if no score given; error is outside of bounds. Returns a number."
  (let* ((score (xprompt (format "%s (0-100)" (or msg "Rating")))))
    (if (string-blank-p score)
        nil
      (let* ((parsed (cl-parse-integer score)))
        (when (or (< parsed 0) (> parsed 100))
          (error (format "score must be between 0-100" parsed)))
        parsed))))

(defun xprompt-year (&optional msg)
  "Prompt the user for a year between -9999 and 9999. Returns nil
if no input given."
  (let* ((year (xprompt (or msg "Year"))))
    (if (string-blank-p year)
        nil
      (let* ((parsed (cl-parse-integer year)))
        (when (or (< parsed -9999) (> parsed 9999) )
          (error (format "Year must be -9999 < n < 9999: %d" parsed)))
        parsed))))
