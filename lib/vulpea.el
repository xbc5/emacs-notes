(defun my/prompt-for-aliases()
  "Prompt for multiple aliases, separated by ';'.
Don't worry about leading or trailing spaces."
  (s-join " " (mapcar (lambda (s)
                        (format "\"%s\"" (s-trim s)))
                      (s-split ";" (read-string "Alias: ") t))))

;; credit to nobiot
(defun my/capture-prompt (title)
  "Prompt the user to choose a capture template."
  (funcall (nth 2 (smenu-dispatch my/capture-switch)) title))

(cl-defun my/roam-node-find (&optional other-window initial-input filter-fn pred &key templates)
  (interactive current-prefix-arg)
  (let ((node (org-roam-node-read initial-input filter-fn pred))) ; fuzzy find
    (if (org-roam-node-file node) ; if found
        (org-roam-node-visit node other-window) ; use
      (my/capture-prompt (org-roam-node-title node))))) ; create

(defun my/vulpea-props (&rest args)
  "Return a list of cons cells for use in :properties.
This allows you to dynamically exclude unused props."
  (let* ((type (plist-get args :type))
         (cat  (plist-get args :cat))
         (aliases  (plist-get args :aliases))
         (rating  (plist-get args :rating))
         (state  (plist-get args :state))
         (year (plist-get args :year))
         (roamrefs  (plist-get args :roamrefs))
         (props '()))
    (unless (eq type nil)
      (setf props (cons (cons "NOTE_TYPE" type) props)))
    (unless (eq cat nil)
      (setf props (cons (cons "NOTE_CATEGORY" cat) props)))
    (unless (eq aliases nil)
      (setf props (cons (cons "ROAM_ALIASES" aliases) props)))
    (unless (eq rating nil)
      (setf props (cons (cons "RATING" rating) props)))
    (unless (eq state nil)
      (setf props (cons (cons "STATE" state) props)))
    (unless (eq year nil) ; is number or nil
      (setf props (cons (cons "YEAR" year) props)))
    (unless (eq roamrefs nil)
      (setf props (cons (cons "ROAM_REFS" roamrefs) props)))
    props))

(defun my/rating-prompt (&optional msg)
  "Prompt the user for a rating between 0-100. Return nil
if no score given; error is outside of bounds. Returns a number."
  (let* ((score (my/prompt (format "%s (0-100)" (or msg "Rating")))))
    (if (string-blank-p score)
        nil
      (let* ((parsed (cl-parse-integer score)))
        (when (or (< parsed 0) (> parsed 100))
          (error (format "score must be between 0-100" parsed)))
        parsed))))

(defun my/year-prompt (&optional msg)
  "Prompt the user for a year between -9999 and 9999. Returns nil
if no input given."
  (let* ((year (my/prompt (or msg "Year"))))
    (if (string-blank-p year)
        nil
      (let* ((parsed (cl-parse-integer year)))
        (when (or (< parsed -9999) (> parsed 9999) )
          (error (format "Year must be -9999 < n < 9999: %d" parsed)))
        parsed))))
