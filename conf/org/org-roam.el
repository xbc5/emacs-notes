(map! :leader
      :prefix "r"
      :desc "org-roam-node-find" "f" #'org-roam-node-find
      :desc "org-roam-alias-add" "a" #'org-roam-alias-add
      :desc "org-roam-tag-add" "t" #'org-roam-tag-add
      :desc "org-roam-refile" "rf" #'org-roam-refile
      :desc "org-roam-buffer-toggle" "l" #'org-roam-buffer-toggle
      :desc "org-roam-graph" "g" #'org-roam-graph
      :desc "org-roam-node-insert" "i" #'org-roam-node-insert
      :desc "org-roam-capture" "c" #'org-roam-capture)


;; batch all SQL operations as a single transaction (fixes slow file saves).
(advice-add 'org-roam-db-update-file :around
              (defun +org-roam-db-update-file (fn &rest args)
                  (emacsql-with-transaction (org-roam-db)
                    (apply fn args))))

(setq org-roam-directory "~/org"
      org-roam-completion-everywhere t
      org-roam-tag-sources '(prop all-directories)
      org-roam-file-completion-tag-position 'append ;; 'prepend | 'append | 'omit
      +org-roam-open-buffer-on-find-file nil)  ;; disable auto-loading of backlinks

(setq org-roam-capture-templates '(("i" "idea" plain "\n\n* meta\n* related\n* details\n%?\n* conclusion"
                                    :unnarrowed t
                                    :target (file+head "%<%Y%m%d%H%M%S>.org"
                                                        "#+title: ${title}"))
                                  ("b" "bib notes" plain "\n\n* meta\n* conclusions\n* summary\n* notes\n%?\n* thoughts\n"
                                   :unnarrowed t
                                   :target (file+head "bib/notes/%<%Y%m%d%H%M%S>.org"
                                                      ":PROPERTIES:\n:ROAM_REFS: cite:${citekey}\n:AUTHORS: ${author}\n:END:\n#+title: ${title}\n#+filetags: :bib:"))))

(add-hook 'after-init-hook 'org-roam-mode)
(org-roam-db-autosync-mode)
