(setq doom-font (font-spec :family "monospace" :size 15 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 13)
      doom-theme 'doom-one
      display-line-numbers-type t
      org-directory "~/org"
      my/bib (concat org-directory "/bib")
      my/refs (concat my/bib "/refs.bib" )
      my/bib-notes (concat my/bib "/notes"))

(map! :desc "completion-at-point" "<M-SPC>" #'completion-at-point)
