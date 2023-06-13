(defun my/get-bibtex-key (node)
  "A BibTeX key picker using completing-read."
  (completing-read "Citation key: "
                   (mapcar #'(lambda (x) (cdr (assoc "=key=" x)))
                           (bibtex-completion-candidates))))

(defun my/ls-agenda-dir ()
  "Get a directory listing of agenda files inside the agenda directory."
  (directory-files my/org-agenda-dir nil org-agenda-file-regexp)) ; NEVER return full path

(defun my/fix-org-file-name (name)
  "Ensure that there is an org extension, and replace spaces with underscores."
  (let* ((tidy (replace-regexp-in-string "\\.org$" ""
                                         (replace-regexp-in-string " " "_" name))))
    (concat tidy ".org")))

(defun my/abs-agenda-fpath (fname)
  "Given an agenda file name, return the full, absolute path."
  (concat my/org-agenda-dir fname))

(defun my/pick-agenda-file ()
  "Pick an agenda file from a completion list, otherwise create it."
  ;; DON'T create files in the agenda directory WITH SPACES or WITHOUT
  ;; the .ORG EXTENSION. To be safe: use this picker when creating files too.
  (my/abs-agenda-fpath
   (my/fix-org-file-name
    (completing-read "Choose an agenda file: " (my/ls-agenda-dir)) )))

(defun my/find-agenda-file ()
  "Find an existing agenda file, or create one."
  (interactive)
  (mkdir my/org-agenda-dir t)
  (set-buffer
   (org-capture-target-buffer
    (my/pick-agenda-file)))
  (goto-char (point-max)))

(defun my/read-file (file)
  "Read the contents of a file."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun my/template-path (name)
  "Return the full template path for the given file name (without extension)."
  (concat my/templates-dir "/" name ".org"))

(defun my/template (name)
  "Return the template contents for the given extensionless file name."
  (my/read-file (my/template-path name)))

(defun my/task-template ()
  "Return the task template contents. Use this for capture templates."
  (my/template "task"))

(defun my/bookmark-template ()
  "Return the bookmark template contents. Use this for capture templates."
  (my/template "bookmark"))

(defun my/open-stackoverflow-question (path)
  "Open a link to a StackOverflow question. An SO link will end in useless text,
  only the ID matters: e.g. https://stackoverflow.com/questions/12345/ignore-this-part"
  (browse-url (format "https://stackoverflow.com/questions/%s" path)))
(defun my/open-stackoverflow-answer (path)
  "Open a link to a specific StackOverflow answer within a page."
  (browse-url (format "https://stackoverflow.com/a/%s" path)))
(defun my/open-rfc-link (path)
  "Open IETF docs given only a number > 0."
  (browse-url (format "https://tools.ietf.org/html/rfc%s" path)))
(defun my/open-coinmarketcap-link (path)
  "Open CMC token page."
  (browse-url (format "https://coinmarketcap.com/currencies/%s" path)))
(defun my/open-reddit-link (path)
  "Open Reddit page."
  (browse-url (format "https://www.reddit.com/%s" path)))
(defun my/open-caniuse-link (path)
  "Open Can I Use reference."
  (browse-url (format "https://caniuse.com/?search=%s" path)))
(defun my/open-mdncss-link (path)
  "Open an MDN CSS reference page."
  (browse-url (format "https://developer.mozilla.org/en-US/docs/Web/CSS/%s" path)))
(defun my/open-hn-link (path)
  "Open an HN link."
  (browse-url (format "https://news.ycombinator.com/item?id=%s" path)))

(defun my/get-agenda-filter (key)
  "Given a key, return the associated agenda filter string."
  (s-join "" (cdr (assoc key my/agenda-filters))))

(defun my/get-alist-keys (lst)
  "Given an alist, return a list of its keys."
  (mapcar (lambda (el) (car el)) lst))

(defun my/pick-agenda-filter ()
  "Prompt the user to pick a premade filter defined in my/agenda-filters."
  (my/get-agenda-filter
   (completing-read "Tag: " (my/get-alist-keys my/agenda-filters))))

(defun my/set-agenda-filter ()
  "Prompt the user to pick and apply a premade filter defined in my/agenda-filters."
  (interactive)
  (org-tags-view t (my/pick-agenda-filter)))


;; This code is for subdirectory projects
;; (setq org-roam-capture-templates
;;        '(("d" "default" plain
;;           #'org-roam-capture--get-point
;;          "%?"
;;           :file-name "%(+org-notes-subdir)/%<%Y%m%d%H%M%S>-${slug}"
;;           :head "#+TITLE: ${title}\n#+TIME-STAMP: <>\n\n"
;;           :unnarrowed t)))

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
