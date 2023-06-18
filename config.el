;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; misc
(load! "conf/global")
(load! "lib/shims")
(load! "lib/util")

;; pkg
(load! "conf/plantuml-mode")
(load! "conf/helm-bibtex")
(load! "conf/spell")
(load! "conf/org")
(load! "conf/org-ref")
(load! "conf/org-roam")
(load! "conf/org-ql")
(load! "conf/org-fancy-priorities")
(load! "conf/nursery")
(load! "conf/org-roam-bibtex")
;;(load! "conf/org-roam-server")

(org-roam-mode)
