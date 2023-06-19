;; WARN: loading this after org-roam causes popup messages
;; from magit for some reason.
(map! :leader
      :prefix "r"
      :desc "org-roam-buffer-toggle" "l" #'org-roam-buffer-toggle
      :desc "org-roam-node-insert" "i" #'org-roam-node-insert)

(setq my/roam-templates
      ;; least used letters in the alphabet: (1/1111) zqjxkvbywgp (1/47)
      '(("p" "permanent note" plain "\n\n* meta\n* related\n* brief\n* summary\n* conclusion\n* details\n%?"
         :unnarrowed t
         :target (file+head "perm/%<%Y%m%d%H%M%S>.org"
                            "#+title: ${title}"))
        ("e" "encrypted note" plain "\n\n* meta\n* related\n* brief\n* summary\n* conclusion\n* details\n%?"
         :unnarrowed t
         :target (file+head "secure/%<%Y%m%d%H%M%S>.org.gpg"
                            "#+title: ${title}"))
        ("v" "quote" plain (function (lambda () (my/template "quote")))
         :unnarrowed t
         :target (file+head "quote/%<%Y%m%d%H%M%S>.org"
                            "#+title: ${title}"))
        ("l" "literature note" plain "\n\n* meta\n* conclusions\n* summary\n* notes\n%?\n* thoughts\n"
         :unnarrowed t
         :target (file+head "lit/%<%Y%m%d%H%M%S>.org"
                            ":PROPERTIES:\n:ROAM_REFS: cite:${citekey}\n:AUTHORS: ${author}\n:END:\n#+title: ${title}\n#+filetags: :bib:"))))

(defun my/org-roam-node-find-split ()
  "Perform a Roam node find, but open the buffer in a split."
  (interactive)
  (org-roam-node-find t))

(use-package! org-roam
  :bind (("M-n" . #'org-roam-node-find)
         ("M-N" . #'my/org-roam-node-find-split)
         ("M-Y" . #'org-roam-alias-add)
         ("M-R" . #'org-roam-refile)
         ("M-T" . #'org-roam-tag-add))
  :init (make-directory org-roam-directory t)
  :config (setq org-roam-completion-everywhere t
                org-roam-capture-templates my/roam-templates)
  :hook ((after-init . org-roam-mode)
         (org-roam-mode . org-roam-db-autosync-mode)))
