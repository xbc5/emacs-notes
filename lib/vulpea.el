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
