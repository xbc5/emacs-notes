;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; misc
(load! "conf/global")
(load! "lib/shims")
(load! "lib/xtype")
(load! "lib/xstr")
(load! "lib/xnum")
(load! "lib/xseq")
(load! "lib/util")
(load! "lib/xtag")
(load! "lib/smenu")
(load! "lib/fs")
(load! "lib/net")
(load! "lib/prompt")
(load! "lib/error")
(load! "lib/xorg")
(load! "lib/roam")
(load! "lib/img")
(load! "lib/xlicense")
(load! "lib/vulpea")
(load! "lib/xquote")
(load! "lib/xname")

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
