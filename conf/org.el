;; -*- lexical-binding: t; -*-
(make-directory org-directory t)

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

(after! org
  ;;(add-hook 'org-mode-hook #'xorg--rename-org-files)
  (add-to-list 'org-modules 'ol-info) ;; for 'info:' links
  (setq org-startup-folded t
        org-image-actual-width (list 800) ; default img width; use a list to enable ATTR fallbacks
        org-cycle-max-level 2))
