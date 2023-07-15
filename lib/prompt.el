;; -*- lexical-binding: t; -*-
;; BUG: required? does not pass through to required-match,
;; this is a bug with completing-read-multiple, and not it's
;; not the s-trim advice that you've added.
(defun xprompt-crm (prompt tag-fname &optional required?)
  (xtag-write tag-fname
              (completing-read-multiple
               (concat prompt ": ") (xtag-get tag-fname) nil required?)))

(defun xprompt (prompt &optional required?)
  "Prompt with PROMPT. If REQUIRED then error on
blank string.

Returns string, or nil (if no input)."
  (let* ((txt (s-trim (read-string (concat prompt ": ")))))
    (when required?
      (xcheck "You MUST provide a value" (not (string-blank-p txt))))
    (if (string-blank-p txt) nil txt)))

(defun xprompt-imdb-id (prompt &optional required?)
  "Prompt for a IMDb ID and validate it. If REQUIRED then
error on blank string."
  (let ((id (xprompt prompt required?)))
    (unless (eq id nil)
      (xcheck (cons "Invalid IMDb ID" id)
              (string-match
               "^\\(tt\\|nm\\|co\\|ev\\|ch\\|ni\\)[0-9]\\{7,8\\}$"
               (s-trim id))))
    id))

(defun xprompt-url (prompt &optional required?)
  "Prompt for a URL and validate it. If REQUIRED then
error on blank string."
  (let ((url (xprompt prompt required?))
        (err "You MUST provide a secure URL, not"))
    (unless (eq url nil)
      (xcheck (cons err url) (xurl-https? url)))
    url))

(defun xprompt-yt (prompt &optional required?)
  "Prompt for a URL and validate it. If REQUIRED then
error on blank string."
  (let ((url (xprompt prompt required?))
        (err "You MUST provide a YouTube URL, not"))
    (unless (eq url nil)
      (xcheck (cons err url) (xurl-yt? url)))
    url))

(defun xprompt-multi (prompt &optional required?)
  "Prompt for multiple values, separated by ';'.
This will return a neat list or nil."
  (let* ((v (xprompt prompt required?)))
    (if v
        (seq-filter #'xstr-t (xstr-neat (s-split ";" v)))
      v)))

(defun xprompt-aliases (&optional required closure)
  "Prompt for aliases. This is a convenience function,
and only exists to keep logic clean within capture
templates (i.e. returning a CLOSURE).

CLOSURE means return a lambda to the prompt instead."
  (if closure
      (lambda () (xprompt-multi "Aliases" required))
    (xprompt-multi "Aliases" required)))

(defun xprompt-rating (&optional msg required?)
  "Prompt the user for a rating between 0-100. Return nil
if no score given; error is outside of bounds. Returns a number."
  (let* ((score (xprompt (format "%s (0-100)" (or msg "Rating")) required?)))
    (if (eq nil score)
        nil
      (let* ((parsed (cl-parse-integer score)))
        (when (or (< parsed 0) (> parsed 100))
          (error (format "score must be between 0-100" parsed)))
        parsed))))

(defun xprompt-year (&optional msg required?)
  "Prompt the user for a year between -9999 and 9999. Returns nil
if no input given."
  (let* ((year (xprompt (or msg "Year") required?)))
    (if (eq nil year)
        nil
      (let* ((parsed (cl-parse-integer year)))
        (when (or (< parsed -9999) (> parsed 9999) )
          (error (format "Year must be -9999 < n < 9999: %d" parsed)))
        parsed))))
