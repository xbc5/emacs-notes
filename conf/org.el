;; -*- lexical-binding: t; -*-
(use-package! org
  :init
  (make-directory org-directory t)
  (map! "M-M" #'org-capture)
  :config
  (add-to-list 'org-modules 'ol-info) ; Enable 'info:' links.
  (setq org-startup-folded t
        org-image-actual-width (list 800) ; Use a list to enable ATTR fallbacks.
        org-cycle-max-level 2))
