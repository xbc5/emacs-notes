(make-directory org-directory t)

(defun shim/set-org-agenda-files ()
  (interactive)
  (setq org-agenda-files (directory-files xorg-agenda-dir t "\\.org$"))) ; set outside of after! as per the manual

(shim/set-org-agenda-files)

(map! "M-M" #'org-capture
      "M-i" #'ximg-insert-org
      :leader
      :prefix "m"
      :desc "Apply custom agenda filters" "t" 'my/set-agenda-filter)

(defun xorg--rename-org-files ()
  "Rename the current ORG file no save: use the ORG
title. This will work for Roam too, because xname-change
will use rename-file, which in turn has Roam advice to
autosync the database."
  (add-hook 'before-save-hook 
            (lambda () 
              (xname-change (concat (org-get-title) ".org"))) nil 'local))

;; org is a module, and we're reconfiguring it here (i.e. don't use use-package!)
(after! org
  (add-hook 'org-mode-hook #'xorg--rename-org-files)
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
        org-image-actual-width 800 ; default img width
        org-agenda-file-regexp "^.*\\.org$"
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
                                ("l" "Bookmark" entry (function xorg-agenda-file-find) (function (lambda () (my/template "bookmark"))))
                                ("b" "Book" entry (function xorg-agenda-file-find) (function (lambda () (my/template "book"))))
                                ("t" "Task" entry (function xorg-agenda-file-find) (function (lambda () (my/template "task")))))
        org-highest-priority 65
        org-lowest-priority 69
        org-default-priority 68
        org-priority-faces '((65 :foreground "red" :weight bold)
                             (66 :foreground "orange" :weight bold)
                             (67 :foreground "yellow" :weight bold)
                             (68 :foreground "green" :weight bold)
                             (69 :foreground "#2a7286" :weight bold))))
