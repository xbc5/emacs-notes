;; -*- lexical-binding: t; -*-
(make-directory org-directory t)
(make-directory xorg-agenda-dir t)

(map! "M-M" #'org-capture
      "M-i" #'ximg-insert-org)

(defun xorg--rename-org-files ()
  "Rename the current ORG file on save: use the ORG
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
        org-image-actual-width (list 800) ; default img width; use a list to enable ATTR fallbacks
        org-cycle-max-level 2
        org-refile-targets '((nil :maxlevel . 3)
                             (("*.org") :maxlevel . 3)) ; refile to other files
        org-outline-path-complete-in-steps nil ; refile in a single go
        org-refile-use-outline-path t ; show file path for refiling
        org-todo-keywords '((sequence "TODO(t)" "STRT(i)" "WAIT(w)" "|" "DONE(d)" "DROP(c)"))
        org-todo-keyword-faces
        '(("TODO" :foreground "#16cafa")
          ("NEXT" :foreground "magenta")
          ("WAIT" :foreground "brown")
          ("DONE" :foreground "#666666")
          ("DROP" :foreground "#666666"))
        (append org-capture-templates
                '(("f" "Fleeting Note" entry (file+olp+datetree "fleeting.org") "* %?")))
        ))
