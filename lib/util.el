(defun my/pick-bibtex-key (&optional node)
  "A BibTeX key picker using completing-read."
  (completing-read "Citation key: "
                   (mapcar #'(lambda (x) (cdr (assoc "=key=" x)))
                           (bibtex-completion-candidates))))

(defun my/template-path (name)
  "Return the full template path for the given file name."
  (f-join my/templates-dir (concat name ".org")))

(defun my/template (name)
  "Return the template contents for the given extensionless file name."
  (my/read-file (my/template-path name)))

(defun my/slugify (str &optional trail)
  "Take str, trim it, and replace all spaces with underscores."
  (replace-regexp-in-string " +" (or trail "_") (s-trim str)))

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
(defun my/open-twitter-link (path)
  "Open a Tweet."
  (browse-url (format "https://www.twitter.com/%s" path)))

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

(defun my/file-exists-p (path)
  "file-exists-p returns t for blank strings, this fixes that."
  (and (not (string-blank-p path)) (file-exists-p path)))

(defun xecho (arg)
  "Simply echo the one ARG passed in.
Useful in cases that require a lambda
to mutate and return something, but you
do not want to mutate it."
  arg)

(defun this-or-that (this that)
  "Return the value of the symbol: THIS (if it's
defined and not nil); otherwise return THAT.

THIS is a symbol; THAT is any value."
  (if (xnil-ish this) that (symbol-value this)))

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
