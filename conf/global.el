(setq doom-font (font-spec :family "monospace" :size 15 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 13)
      doom-theme 'doom-one
      xname-max-len 130 ; remember rsync max len is 136
      confirm-kill-processes nil ; roam-server etc
      confirm-kill-emacs nil ; will still confirm for unsaved buffers
      use-package-verbose t ; show package config in details in messages
      display-line-numbers-type t
      org-directory "~/org"
      org-roam-directory org-directory
      ;; WARN: f-join and other utils won't work here, it's too early in the boot.
      xtag-files (concat org-roam-directory "/tags")
      xlicense-dpath xtag-files
      xlicense-fpath (concat xlicense-dpath "/license-types.yaml")
      my/templates-dir (concat doom-user-dir "/templates") ; e.g. org capture templates
      my/org-agenda-dir (concat org-directory "/agenda")
      my/bib (concat org-roam-directory "/bib" )
      my/imgs (concat org-roam-directory "/img")
      my/bib-file-re "\.\\(bib\\|org\\)$" ; match files in bib dir
      my/bib-files (directory-files my/bib t my/bib-file-re) ; bib/*.{org,bib}
      my/lit-notes (concat org-roam-directory "/lit"))

(let* ((active "-TODO=\"DONE\"-TODO=\"DROP\"")
       (books "+SOURCE_TYPE=\"book\"|+book|+books")
       (immediate "+PRIORITY=\"A\""))
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
              (list "immediate [active,all]" immediate active)
              (list "UAP [active]" "+UAP" active)
              (list "books [active]" books active)
              (list "Emacs [active]" "+Emacs|+orgroam|+orgmode|+elisp" active))))

(map! :desc "completion-at-point" "<M-SPC>" #'completion-at-point)

(advice-add
 'completing-read-multiple
 :filter-return
 (lambda (lst)
   "Trim each returned element."
   (mapcar #'s-trim lst)))
