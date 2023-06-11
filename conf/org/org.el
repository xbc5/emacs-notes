(use-package! org
  :config
  (make-directory "~/org" t)  ;; t to ignore 'exists' error
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
        org-agenda-files '("~/org/agenda") ; BUG(#19): should work, but doesn't: dir not in Org mode
        org-agenda-file-regexp "^.*\\.org$" ; should apply to ^ dir only
        org-cycle-max-level 2
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
                                ("a" "Agenda" entry (function my/find-agenda-file) (function my/agenda-template)))
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
