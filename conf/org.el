(map! :leader
      :prefix "m"
      :desc "Apply custom agenda filters" "t" 'my/set-agenda-filter)

(use-package! org
  :bind ("M-M" . org-capture)

  :init (make-directory org-directory t)

  :config
  (add-to-list 'org-modules 'ol-info) ;; for 'info:' links
  (org-add-link-type "RFC" 'my/open-rfc-link)
  (org-add-link-type "CMC" 'my/open-coinmarketcap-link)
  (org-add-link-type "caniuse" 'my/open-caniuse-link)
  (org-add-link-type "reddit" 'my/open-reddit-link)
  (org-add-link-type "HN" 'my/open-hn-link)
  (org-add-link-type "SOQ" 'my/open-stackoverflow-question)
  (org-add-link-type "SOA" 'my/open-stackoverflow-answer)
  (org-add-link-type "twitter" 'my/open-twitter-link)
  (setq org-startup-folded t
        org-agenda-files '(my/org-agenda-dir) ; BUG(#19): should work, but doesn't: dir not in Org mode
        org-agenda-file-regexp "^.*\\.org$" ; should apply to ^ dir only
        org-cycle-max-level 2
        org-refile-targets '((nil :maxlevel . 3)
                             (("*.org") :maxlevel . 3)) ; refile to other files
        org-outline-path-complete-in-steps nil ; refile in a single go
        org-refile-use-outline-path t ; show file path for refiling
        org-todo-keywords '((sequence "TODO(t)" "STRT(i)" "WAIT(w)" "|" "DONE(d)" "DROP(c)"))
        org-todo-keyword-faces
        '(("TODO" :foreground "#16cafa")
          ("STRT" :foreground "magenta")
          ("WAIT" :foreground "brown")
          ("DONE" :foreground "#666666")
          ("DROP" :foreground "#666666"))
        org-capture-templates '(("f" "Fleeting notes" entry (file+olp+datetree "fleeting.org") "* %?")
                                ("b" "Bookmark" entry (function my/find-agenda-file) (function my/bookmark-template))
                                ("t" "Task" entry (function my/find-agenda-file) (function my/task-template)))
        org-highest-priority 65
        org-lowest-priority 69
        org-default-priority 68
        org-priority-faces '((65 :foreground "red" :weight bold)
                             (66 :foreground "orange" :weight bold)
                             (67 :foreground "yellow" :weight bold)
                             (68 :foreground "green" :weight bold)
                             (69 :foreground "#2a7286" :weight bold))))
