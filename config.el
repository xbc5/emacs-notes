;; -*- lexical-binding: t; -*-

;; ----- GLOBAL SETTINGS ----------------------------------------------
;; Used universally by multiple packages.
(setq org-directory "~/org")
(require 'org) ;; Central to everyhing I do.

;; - EMOJIS -
;; The following code will display emojis (e.g., in mu4e) by setting
;; emoji-specific fonts as fallback fonts for 'emoji- and `symbol-specific
;; characters.
;;
;; 1. sudo dnf install google-noto-emoji-color-fonts
;; 1. fc-cache -fv
;;
;; `'set-fontset-font' is for setting fallback fonts. They also don't
;; interfere with Nerd fonts.
(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'prepend)
  (set-fontset-font t 'emoji "Noto Color Emoji" nil 'prepend))

;; ----- CONFIG -------------------------------------------------------
(load! "conf/org")
(load! "conf/org-node")

(when (string= (getenv "EMACS_MODE") "email")
  (load! "conf/mu4e"))

;; ----- CUSTOM PACKAGES ----------------------------------------------
;; Add all packages/ to the load path.
(let ((default-directory (concat doom-user-dir "packages/")))
  (normal-top-level-add-subdirs-to-load-path))

;; (require 'ht)
;; (require 'f)

;; (load! "conf/neutron")
;; (require 'neutron)

;; UTIL --------------------------------------------------------------
;; (load! "conf/global")
;; (load! "lib/xht")
;; (load! "lib/fs")
;; (load! "lib/util")
;; (load! "lib/xui")
;; (load! "lib/xtype")
;; (load! "lib/xstr")
;; (load! "lib/xseq")
;; (load! "lib/xnum")
;; (load! "lib/email")
;;
;; ;; LIB ---------------------------------------------------------------
;; (load! "lib/shims")
;; (load! "lib/xtag")
;; (load! "lib/smenu")
;; (load! "lib/net")
;; (load! "lib/prompt")
;; (load! "lib/error")
;; (load! "lib/xorg")
;; ;; (load! "lib/roam")
;; (load! "lib/img")
;; (load! "lib/xlicense")
;; (load! "lib/xname")
;; (load! "lib/xtime")
;; (load! "lib/xdrill")
;; (load! "lib/xbeancount")

;; CONFIG ------------------------------------------------------------
;; (load! "conf/plantuml-mode")
;; (load! "conf/spell")
;;(load! "conf/gtd") ; It's better to load after conf/org.el, but it's not necessary.
;; (load! "conf/refs") ; slow boot
;; (load! "conf/org-roam")
;; (load! "conf/org-drill")
;; (load! "conf/org-ql")
;; (load! "conf/org-fancy-priorities")
;; (load! "conf/nursery")
;; (load! "conf/org-roam-server")

;; WARN: slows down boot by ~6s
;; (require 'org-roam)
;; (org-roam-mode)

