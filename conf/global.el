; third-party
(setq org-directory "~/org"
      org-roam-directory org-directory)

;; The number of CPU cores that native compile uses. 
(setq native-comp-async-jobs-number (num-processors))

;; How many optimizations native-comp should employ.
;;   -1 = no native-compilation at all
;;    0 = no optimizations at all
;;    1 = light optimizations
;;    2 = all (but only safe) optimizations  (the default)
;;    3 = all (including unsafe) optimizations
(setq native-comp-speed 2)

;; Or tell native-comp to ignore certain, problematic files
;; by regular expression.
;;(add-to-list 'native-comp-jit-compilation-deny-list "/some-file\\.el\\'")

; x paths
(setq my/bib (concat org-roam-directory "/bib" )
      my/imgs (concat org-roam-directory "/img")
      my/lit-notes (concat org-roam-directory "/lit")
      my/bib-file-re "\.\\(bib\\|org\\)$" ; match files in bib dir
      my/templates-dir (concat doom-user-dir "/templates") ; e.g. org capture templates
      xorg-agenda-dir (concat org-directory "/agenda")
      xtag-files (concat org-roam-directory "/tags")
      xlicense-dpath xtag-files
      xlicense-fpath (concat xlicense-dpath "/license-types.yaml"))

; neccessary for other parts to work
(make-directory org-directory t) ; for next line
(make-directory my/bib t) ; for my/bib-files

(setq my/bib-files (directory-files my/bib t my/bib-file-re)) ; bib/*.{org,bib}

; x props
(setq xname-max-len 130) ; remember rsync max len is 136

(setq doom-font (font-spec :family "monospace" :size 15 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 13)
      doom-theme 'doom-one
      confirm-kill-processes nil ; roam-server etc
      confirm-kill-emacs nil ; will still confirm for unsaved buffers
      use-package-verbose t ; show package config in details in messages
      display-line-numbers-type t)

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
              (list "personal [active]" "+personal" active)
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
