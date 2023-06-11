(setq doom-font (font-spec :family "monospace" :size 15 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 13)
      doom-theme 'doom-one
      confirm-kill-processes nil ; roam-server etc
      confirm-kill-emacs nil ; will still confirm for unsaved buffers
      use-package-verbose t ; show package config in details in messages
      display-line-numbers-type t
      org-directory "~/org"
      org-roam-directory org-directory
      my/templates-dir (concat doom-user-dir "/templates") ; e.g. org capture templates
      my/org-agenda-dir (concat org-directory "/agenda/")
      my/bib (concat org-directory "/bib")
      my/refs (concat my/bib "/refs.bib" )
      my/bib-notes (concat my/bib "/notes"))

(map! :desc "completion-at-point" "<M-SPC>" #'completion-at-point)
