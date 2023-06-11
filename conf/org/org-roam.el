;; WARN: loading this after org-roam causes popup messages
;; from magit for some reason.
(map! :leader
      :prefix "r"
      :desc "org-roam-alias-add" "a" #'org-roam-alias-add
      :desc "org-roam-tag-add" "t" #'org-roam-tag-add
      :desc "org-roam-refile" "rf" #'org-roam-refile
      :desc "org-roam-buffer-toggle" "l" #'org-roam-buffer-toggle
      :desc "org-roam-graph" "g" #'org-roam-graph
      :desc "org-roam-node-insert" "i" #'org-roam-node-insert
      :desc "org-roam-capture" "c" #'org-roam-capture)

(setq my/roam-templates
      '(("i" "idea" plain "\n\n* meta\n* related\n* details\n%?\n* conclusion"
         :unnarrowed t
         :target (file+head "%<%Y%m%d%H%M%S>.org"
                            "#+title: ${title}"))
        ("b" "bib notes" plain "\n\n* meta\n* conclusions\n* summary\n* notes\n%?\n* thoughts\n"
         :unnarrowed t
         :target (file+head "bib/notes/%<%Y%m%d%H%M%S>.org"
                            ":PROPERTIES:\n:ROAM_REFS: cite:${citekey}\n:AUTHORS: ${author}\n:END:\n#+title: ${title}\n#+filetags: :bib:"))))

(use-package! org-roam
  :bind (("M-n" . org-roam-node-find))
  :init (make-directory org-roam-directory t)
  :config (setq org-roam-completion-everywhere t
                org-roam-capture-templates my/roam-templates)
  :hook ((after-init . org-roam-mode)
         (org-roam-mode . org-roam-db-autosync-mode)))
