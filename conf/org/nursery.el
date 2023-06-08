; auto-update dynamic blocks
(use-package org-roam-dblocks
             :hook (org-mode . org-roam-dblocks-autoupdate-mode))

(map! :leader
      :prefix "r"
      :desc "org-insert-dblock:notes" "d" #'org-insert-dblock:notes)

