;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; misc
(load! "conf/global")
(load! "lib/shims")
(load! "lib/types")
(load! "lib/util")
(load! "lib/smenu")
(load! "lib/fs")
(load! "lib/net")
(load! "lib/error")
(load! "lib/roam")
(load! "lib/img")
(load! "lib/vulpea")

;; pkg
(load! "conf/plantuml-mode")
(load! "conf/spell")
(load! "conf/org")
(load! "conf/refs") ; slow boot
(load! "conf/org-roam")
(load! "conf/vulpea")
(load! "conf/org-ql")
(load! "conf/org-fancy-priorities")
(load! "conf/nursery")
;(load! "conf/org-roam-server")

; WARN: slows down boot by ~6s
; (require 'org-roam)
; (org-roam-mode)
