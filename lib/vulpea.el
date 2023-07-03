;; credit to nobiot
(defun my/vulpea-capture-prompt (node)
  "Prompt the user to choose a capture template.
NODE is a Roam node."
  (funcall (nth 0 (smenu "Template type" my/capture-switch)) node))

(cl-defun my/vulpea-node-find (&optional other-window initial-input filter-fn pred &key templates)
  (interactive current-prefix-arg)
  (let ((node (org-roam-node-read initial-input filter-fn pred))) ; fuzzy find
    (if (org-roam-node-file node) ; if found
        (org-roam-node-visit node other-window) ; use
      (my/vulpea-capture-prompt node)))) ; create

(cl-defun my/vulpea-node-insert (&optional filter-fn)
  "Identical in behaviour to `org-roam-node-insert` except that it
uses the Vulpea capture templates instead.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out."
  (interactive)
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    ;; get markers from active region
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text (org-link-display-format
                                       (buffer-substring-no-properties beg end)))))
               (node (org-roam-node-read region-text filter-fn)) ; find node with text
               (description (or region-text (org-roam-node-formatted node))))
          (if (org-roam-node-id node)
              (progn
                (when region-text ;; if we had any text highlighted
                  (delete-region beg end) ;; remove it and reset markers
                  (set-marker beg nil)
                  (set-marker end nil))
                (let ((id (org-roam-node-id node)))
                  (insert (org-link-make-string (concat "id:" id) description))
                  (run-hook-with-args 'org-roam-post-node-insert-hook id description)))
            (my/vulpea-capture-prompt node))))
    (deactivate-mark))) ; always deactivate

(cl-defun my/vulpea-props (&key type cat aliases rating state year roamrefs contexts artists
                                genres license view-url stream-url download-url info-url)
  "Return a list of cons cells for use in :properties.
This allows you to dynamically exclude unused props."
  (let* ((props '()))
    (unless (xnil-or-blank type)
      (setf props (cons (cons "NOTE_TYPE" type) props)))
    (unless (xnil-or-blank cat)
      (setf props (cons (cons "NOTE_CATEGORY" cat) props)))
    (unless (xnil-or-blank aliases)
      (setf props (cons (cons "ROAM_ALIASES" aliases) props)))
    (unless (eq nil rating)
      (setf props (cons (cons "RATING" rating) props)))
    (unless (xnil-or-blank state)
      (setf props (cons (cons "STATE" state) props)))
    (unless (eq year nil) ; is number or nil
      (setf props (cons (cons "YEAR" year) props)))
    (unless (xnil-or-blank roamrefs)
      (setf props (cons (cons "ROAM_REFS" roamrefs) props)))
    (unless (eq nil artists) ; is list
      (setf props (cons (cons "ARTISTS" (xorg-props artists)) props)))
    (unless (eq nil genres) ; is list
      (setf props (cons (cons "GENRES" (xorg-props genres)) props)))
    (unless (eq nil contexts) ; is list
      (setf props (cons (cons "CONTEXTS" (xorg-props contexts)) props)))
    (unless (xnil-or-blank license)
      (setf props (cons (cons "LICENSE" license) props)))
    (unless (xnil-or-blank view-url)
      (setf props (cons (cons "VIEW_URL" view-url) props)))
    (unless (xnil-or-blank stream-url)
      (setf props (cons (cons "STREAM_URL" stream-url) props)))
    (unless (xnil-or-blank download-url)
      (setf props (cons (cons "DOWNLOAD_URL" download-url) props)))
    (unless (xnil-or-blank info-url)
      (setf props (cons (cons "INFO_URL" info-url) props)))
    props))

(defun xvulpea--tagify (tags)
  "Turns a string or a list of strings, into a list
of tags -- suitable for use in Vulpea :properties.

Returns a list of tagified strings:
  e.g. (\"foo_bar\" ...)"
  (mapcar #'xtag-tagify (xseq-listify tags)))
