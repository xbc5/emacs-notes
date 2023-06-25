(setq my/roam-templates
      ;; WARN: use Vulpea instead; 1 template max here -- literature notes. Org-ref
      ;; will default to this without asking. We don't need special Vulpea features
      ;; for lit notes anyway.
      '(("l" "literature" plain "\n\n* meta\n* conclusions\n* summary\n* notes\n%?\n* thoughts\n"
         :unnarrowed t
         :target (file+head "lit/%<%Y%m%d%H%M%S>.org"
                            ":PROPERTIES:\n:ROAM_REFS: cite:${citekey}\n:AUTHORS: ${author}\n:END:\n#+title: ${title}\n"))))

(defun my/org-roam-node-find-split ()
  "Perform a Roam node find, but open the buffer in a split."
  (interactive)
  (org-roam-node-find t))

(make-directory org-roam-directory t)

;; org-roam is a module, so we reconfigure it
(after! org-roam
  (setq org-roam-completion-everywhere t
        org-roam-capture-templates my/roam-templates)

  (map! "M-n" #'my/roam-node-find
        "M-N" #'my/org-roam-node-find-split
        "M-Y" #'org-roam-alias-add
        "M-R" #'org-roam-refile
        "M-T" #'org-roam-tag-add
        "M-B" #'my/roam-set-brief
        :leader
        :prefix "r"
        :desc "org-roam-buffer-toggle" "l" #'org-roam-buffer-toggle
        :desc "org-roam-node-insert" "i" #'org-roam-node-insert)

  (add-hook 'after-init-hook #'org-roam-mode)
  (add-hook 'org-roam-mode-hook #'org-roam-db-autosync-mode))
