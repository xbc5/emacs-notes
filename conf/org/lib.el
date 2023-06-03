;; This code is for subdirectory projects
;;
;;
;(setq org-roam-capture-templates
;        '(("d" "default" plain
;           #'org-roam-capture--get-point
;          "%?"
;           :file-name "%(+org-notes-subdir)/%<%Y%m%d%H%M%S>-${slug}"
;           :head "#+TITLE: ${title}\n#+TIME-STAMP: <>\n\n"
;           :unnarrowed t)))

;; (defun +org-project-subdir ()
;;   "Select a project subdirectory."
;;   (interactive)
;;   (let ((dirs (cons "."
;;                     (seq-map  ; apply a function to each list item, return a single result
;;                      (lambda (p) ; the function
;;                        (string-remove-prefix org-roam-directory p))  ; remove ~/org from roam dir
;;                      (+file-subdirs (format "%s/project" org-roam-directory) nil t)))))
;;     (completing-read "Project: " dirs nil nil)))

;; (defun +file-subdirs (directory &optional filep rec)
;;   "Return subdirs or files of DIRECTORY according to FILEP.

;; If REC is non-nil then do recursive search."
;;   (let ((res  ; res is a list of files in a directory
;;          (seq-remove
;;           (lambda (file)
;;             (or (string-match "\\`\\."
;;                               (file-name-nondirectory file))
;;                 (string-match "\\`#.*#\\'"
;;                               (file-name-nondirectory file))
;;                 (string-match "~\\'"
;;                               (file-name-nondirectory file))
;;                 ; check if 'file' is a directory => boolean
;;                 (if filep
;;                     (file-directory-p file)             ; does it
;;                   (not (file-directory-p file)))))      ; does it
;;           ; get a list of names in a directory
;;           (directory-files directory t))))
;;     (if rec
;;         (+seq-flatten ; flattent a list of lists -- to a single list
;;          (seq-map (lambda (p) (cons p (+file-subdirs p)))  ; apply a function to each (flattened) list item, get result
;;                   res))
;;       res)))

;; (defun +seq-flatten (list-of-lists)
;;   "Flatten LIST-OF-LISTS."
;;   (apply #'append list-of-lists))
