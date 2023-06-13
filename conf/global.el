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

(let* ((active "-TODO=\"DONE\"-TODO=\"DROP\""))
  (setq my/agenda-filters
        (list (list "AI [active]" "+AI" active)
              (list "study [active]" "+study" active)
              (list "Org-Roam [active]" "+orgroam" active)
              (list "Org-Mode [active]" "+orgmode" active)
              (list "futurology [active]" "+futurology" active)
              (list "dark traits [active]" "+darktraits" active)
              (list "psychology [active]" "+psychology" active)
              (list "Elisp [active]" "+elisp" active)
              (list "privacy [active]" "+privacy" active)
              (list "software [active]" "+software" active)
              (list "anti-aging [active]" "+antiaging" active)
              (list "biotech [active]" "+biotech" active)
              (list "Emacs [active]" "+Emacs|+orgroam|+orgmode|+elisp" active))))

(map! :desc "completion-at-point" "<M-SPC>" #'completion-at-point)
