;;; -*- lexical-binding: t; -*-

(defvar my/elfeed--subscription-dir
  (f-join org-directory "elfeed"))

(defvar my/elfeed--default-file
  (f-join my/elfeed--subscription-dir "default.org"))

(f-mkdir my/elfeed--subscription-dir)

(unless (f-exists-p my/elfeed--default-file)
  (f-write-text ":PROPERTIES:
:ID: 340f9bf0-d703-4798-b510-75339fa81b45
:END:
#+TITLE: default RSS feeds
#+filetags: :elfeed:RSS:feeds:

* Feeds :elfeed:
" 'utf-8 my/elfeed--default-file))

(after! org
  (add-to-list 'org-capture-templates
               '("e" "Elfeed feed file" plain
                 (function org-node-capture-target)
                 "#+filetags: :elfeed:RSS:feeds:\n\n* feeds :elfeed:\n** %?")))

(after! elfeed
  (setq rmh-elfeed-org-files
        (f-glob "*.org" my/elfeed--subscription-dir)))

(setq initial-buffer-choice
      (lambda () (elfeed) (get-buffer "*elfeed-search*")))
