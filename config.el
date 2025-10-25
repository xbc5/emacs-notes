;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(require 'ht)
(require 'vulpea)
(require 'f)

;; UTIL --------------------------------------------------------------
(load! "conf/global")
(load! "lib/xht")
(load! "lib/fs")
(load! "lib/util")
(load! "lib/xui")
(load! "lib/xtype")
(load! "lib/xstr")
(load! "lib/xseq")
(load! "lib/xnum")
(load! "lib/email")

;; LIB ---------------------------------------------------------------
(load! "lib/shims")
(load! "lib/xtag")
(load! "lib/smenu")
(load! "lib/net")
(load! "lib/prompt")
(load! "lib/error")
(load! "lib/xorg")
(load! "lib/roam")
(load! "lib/img")
(load! "lib/xlicense")
(load! "lib/xvulpea")
(load! "lib/xname")
(load! "lib/xtime")
(load! "lib/xdrill")
(load! "lib/xbeancount")

;; CONFIG ------------------------------------------------------------
(load! "conf/plantuml-mode")
(load! "conf/spell")
(load! "conf/org")
(load! "conf/gtd") ; It's better to load after conf/org.el, but it's not necessary.
(load! "conf/refs") ; slow boot
(load! "conf/org-roam")
(load! "conf/org-drill")
(load! "conf/vulpea")
(load! "conf/org-ql")
(load! "conf/org-fancy-priorities")
(load! "conf/nursery")
(when (string= (getenv "EMACS_MODE") "email")
  (load! "conf/mu4e"))
;; (load! "conf/org-roam-server")

;; WARN: slows down boot by ~6s
;; (require 'org-roam)
;; (org-roam-mode)
