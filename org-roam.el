;;; org-roam.el -*- lexical-binding: t; -*-
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


(add-hook 'after-init-hook 'org-roam-mode)


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


(setq org-roam-capture-templates '(("i" "idea" plain "\n* Ideas"
                                    :if-new (file+head "${slug}-%<%Y%m%d%H%M%S>.org"
                                                        "#+title: ${title}")
                                    :unnarrowed t)
                                   ("p" "project idea" plain "\n* Ideas"
                                    :if-new (file+head "project/%(+org-project-subdir)/${slug}-%<%Y%m%d%H%M%S>.org"
                                                       "#+title: ${title}")
                                     :unnarrowed t)
                                  ("b" "bib notes" plain "\n* Notes"
                                     :if-new (file+head "bib/notes/${slug}.org"
                                                        ":PROPERTIES:\n:ROAM_REFS: cite:${slug}\n:END:\n#+title: ${title}")
                                     :unnarrowed t)))

(org-roam-db-autosync-mode)
