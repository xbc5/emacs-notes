;; -*- lexical-binding: t; -*-
(make-directory org-directory t)

(map! "M-M" #'org-capture)

(after! org
  (add-to-list 'org-modules 'ol-info) ;; for 'info:' links
  (setq org-startup-folded t
        org-image-actual-width (list 800) ; default img width; use a list to enable ATTR fallbacks
        org-cycle-max-level 2))
