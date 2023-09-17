(setq my/roam-templates
      ;; WARN: use Vulpea instead; 1 template max here -- literature notes. Org-ref
      ;; will default to this without asking. We don't need special Vulpea features
      ;; for lit notes anyway.
      '(("l" "literature" plain "\n* conclusions\n* summary\n* notes\n%?\n* ideas\n* thoughts\n"
         :unnarrowed t
         :target (file+head "lit/%<%Y%m%d%H%M%S>.org"
                            ":PROPERTIES:\n:ROAM_REFS: cite:${citekey}\n:AUTHORS: ${author}\n:END:\n#+title: ${title}\n"))))

(defun my/roam-node-find-split ()
  "Perform a Roam node find, but open the buffer in a split."
  (interactive)
  (org-roam-node-find t))


(make-directory org-roam-directory t)

;; org-roam is a module, so we reconfigure it
(after! org-roam
  (setq org-roam-completion-everywhere t
        org-roam-capture-templates my/roam-templates)

  (map! "M-n" #'org-roam-node-find
        "M-N" #'my/roam-node-find-split
        "M-Y" #'xroam-prop-set-aliases-root
        "M-R" #'org-roam-refile
        "M-s" #'xroam-status-toggle-root
        "M-S" #'xroam-status-toggle-child
        "M-T" #'org-roam-tag-add
        "M-B" #'xroam-prop-set-brief-root
        "M-I" #'org-roam-node-insert
        :leader
        :prefix "rp"
        :desc "set brief root"      "b" #'xroam-prop-set-brief-root
        :desc "set brief closest"   "B" #'xroam-prop-set-brief-closest
        :desc "set meta root"       "m" #'xroam-prop-set-meta-root
        :desc "set meta closest"    "M" #'xroam-prop-set-meta-closest
        :desc "set aliases root"    "a" #'xroam-prop-set-aliases-root
        :desc "set aliases closest" "A" #'xroam-prop-set-aliases-closest)

  (add-hook 'after-init-hook #'org-roam-mode)
  (add-hook 'org-roam-mode-hook #'org-roam-db-autosync-mode))
