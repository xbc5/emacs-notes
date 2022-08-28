;;; ../.dotfiles/doom/.doom.d/org.el -*- lexical-binding: t; -*-

(make-directory "~/org" t)  ;; t to ignore 'exists' error
;;
;; # Config
(after! org
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
  (setq org-agenda-file-regexp "^.*todos.org$")
  (setq org-startup-folded t
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

;; # todo Priotities
;; use symbols instead of '[#A]' etc.
(use-package! org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '((65 . "1")
                                            (66 . "2")
                                            (67 . "3")
                                            (68 . "4")
                                            (69 . "9"))))
