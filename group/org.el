;;; ../.dotfiles/doom/.doom.d/org.el -*- lexical-binding: t; -*-

;;
;; # Config
(use-package! org
  :config
  (make-directory "~/org" t)  ;; t to ignore 'exists' error
  (defun my/open-rfc-link (path)
    "Open IETF docs given only a number > 0."
    (browse-url (format "https://tools.ietf.org/html/rfc%s" path)))
  (defun my/open-coinmarketcap-link (path)
    "Open CMC token page."
    (browse-url (format "https://coinmarketcap.com/currencies/%s" path)))
  (defun my/open-reddit-link (path)
    "Open Reddit page."
    (browse-url (format "https://libredd.it/%s" path)))
  (defun my/open-caniuse-link (path)
    "Open Can I Use reference."
    (browse-url (format "https://caniuse.com/?search=%s" path)))
  (defun my/open-mdncss-link (path)
    "Open an MDN CSS reference page."
    (browse-url (format "https://developer.mozilla.org/en-US/docs/Web/CSS/%s" path)))
  (defun my/open-hn-link (path)
    "Open an HN link."
    (browse-url (format "https://news.ycombinator.com/item?id=%s" path)))
  (add-to-list 'org-modules 'ol-info) ;; for 'info:' links
  (org-add-link-type "rfc" 'my/open-rfc-link)
  (org-add-link-type "RFC" 'my/open-rfc-link)
  (org-add-link-type "cmc" 'my/open-coinmarketcap-link)
  (org-add-link-type "caniuse" 'my/open-caniuse-link)
  (org-add-link-type "mdn-css" 'my/open-mdncss-link)
  (org-add-link-type "reddit" 'my/open-reddit-link)
  (org-add-link-type "hn" 'my/open-hn-link)
  (org-add-link-type "HN" 'my/open-hn-link)
  (setq org-startup-folded t
        org-agenda-file-regexp "^.*todos.org$"
        org-cycle-max-level 2
        org-directory "~/org"
        org-refile-targets '((nil :maxlevel . 3)
                             (("*.org") :maxlevel . 3))   ;; refile to other files
        org-outline-path-complete-in-steps nil                                       ;; refile in a single go
        org-refile-use-outline-path t                                                ;; show file path for refiling
        org-todo-keywords '((sequence "TODO(t)" "STRT(i)" "WAIT(w)" "|" "DONE(d)" "DROP(c)"))
        org-todo-keyword-faces
        '(("TODO" :foreground "#16cafa")
          ("STRT" :foreground "magenta")
          ("WAIT" :foreground "brown")
          ("DONE" :foreground "#666666")
          ("DROP" :foreground "#666666"))
        org-capture-templates '(("f" "Fleeting notes" entry (file+olp+datetree "fleeting.org") "* %?")
                                ("t" "TODO")
                                ;; TODO: use a variable of some kind
                                ("tb" "Biz" entry (file+headline "todo.org" "Biz") "* TODO [#D] %?")
                                ("td" "Dev" entry (file+headline "todo.org" "Dev") "* TODO [#D] %?")
                                ("tg" "General" entry (file+headline "todo.org" "General") "* TODO [#D] %?"))
        org-highest-priority 65
        org-lowest-priority 69
        org-default-priority 68
        org-priority-faces '((65 :foreground "red" :weight bold)
                             (66 :foreground "orange" :weight bold)
                             (67 :foreground "yellow" :weight bold)
                             (68 :foreground "green" :weight bold)
                             (69 :foreground "#2a7286" :weight bold))))

;; "todo" priorities: use symbols instead of '[#A]' etc.
(use-package! org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '((65 . "1")
                                            (66 . "2")
                                            (67 . "3")
                                            (68 . "4")
                                            (69 . "9"))))

(defun dnd-unescape-uri () nil) ;; BUG: something something undefined

; References and citations
(require 'org-ref-helm)
(use-package! org-ref-helm
  :init (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
	      org-ref-insert-cite-function 'org-ref-cite-insert-ivy
	      org-ref-insert-label-function 'org-ref-insert-label-link
	      org-ref-insert-ref-function 'org-ref-insert-ref-link
	      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))))
(let ((notes-dir "~/org/bib/notes") (bib-file "~/org/bib/refs.bib"))
  (use-package! org-ref
                :config
                (setq org-ref-notes-directory notes-dir
                      org-ref-default-bibliography (list bib-file)))
  (setq bibtex-completion-bibliography bib-file
        bibtex-completion-notes-path notes-dir))
(map! :desc "Insert a citation"
      :map org-mode-map "C-c ]"
      #'org-ref-insert-link-hydra/body)
(after! org-roam-bibtex
        (require 'org-ref)
        (org-roam-bibtex-mode))

(map! :leader
      :prefix "r"
      :desc "org-roam-node-find" "f" #'org-roam-node-find
      :desc "org-roam-alias-add" "a" #'org-roam-alias-add
      :desc "org-roam-tag-add" "t" #'org-roam-tag-add
      :desc "org-roam-refile" "rf" #'org-roam-refile
      :desc "org-roam-buffer-toggle" "l" #'org-roam-buffer-toggle
      :desc "org-roam-graph" "g" #'org-roam-graph
      :desc "org-roam-node-insert" "i" #'org-roam-node-insert
      :desc "org-roam-capture" "c" #'org-roam-capture)


(add-hook 'after-init-hook 'org-roam-mode)


;; batch all SQL operations as a single transaction (fixes slow file saves).
(advice-add 'org-roam-db-update-file :around
              (defun +org-roam-db-update-file (fn &rest args)
                  (emacsql-with-transaction (org-roam-db)
                    (apply fn args))))


(setq org-roam-directory "~/org"
      org-roam-completion-everywhere t
      org-roam-tag-sources '(prop all-directories)
      org-roam-file-completion-tag-position 'append ;; 'prepend | 'append | 'omit
      +org-roam-open-buffer-on-find-file nil)  ;; disable auto-loading of backlinks

; fuzzy select a cite key, parsed from bibtex files
(defun my/get-bibtex-key (node)
  (completing-read "Citation key: "
                   (mapcar #'(lambda (x) (cdr (assoc "=key=" x)))
                           (bibtex-completion-candidates))))

(setq org-roam-capture-templates '(("i" "idea" plain "\n\n* details\n* conclusion"
                                    :unnarrowed t
                                    :target (file+head "${slug}-%<%Y%m%d%H%M%S>.org"
                                                        "#+title: ${title}"))
                                   ("p" "project idea" plain "\n\n* details\n *conclusion"
                                    :unnarrowed t
                                    :target (file+head "project/%(+org-project-subdir)/${slug}-%<%Y%m%d%H%M%S>.org"
                                                       "#+title: ${title}"))
                                  ("b" "bib notes" plain "\n\n* notes\n* thoughts\n* questions"
                                   :unnarrowed t
                                   :target (file+head "bib/notes/${slug}.org"
                                                      ":PROPERTIES:\n:ROAM_REFS: cite:${my/get-bibtex-key}\n:END:\n#+title: ${title}\n#+filetags: :bib:"))))

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

(org-roam-db-autosync-mode)
(after! org-roam-server
  (after! org-roam
    (add-hook 'org-roam-mode-hook 'org-roam-server-mode)) ;; start it when org-roam starts
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))
