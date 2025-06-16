;; -*- lexical-binding: t; -*-
(require 'f)


;; CONFIG VARS--------------------------------------------------------
;; - PATHS -
(setq gtd-dir (f-join org-roam-directory "gtd")
      gtd-active-dir (f-join gtd-dir "active")
      gtd-inactive-dir (f-join gtd-dir "inactive"))


;; UTILS--------------------------------------------------------------
(defun gtd-active-files-set ()
  "Active files contain tasks that should be visible in the agenda view.

This func sets the active files in `org-agenda-files'."
  (setq org-agenda-files (directory-files gtd-active-dir t "\\.org$")))


;; PRE-INITIALISATION-------------------------------------------------
;; - DIRECTORY CREATION -
(make-directory gtd-active-dir t)
(make-directory gtd-inactive-dir t)

(gtd-active-files-set) ;; Set outside of after! as per the manual.

(map! :leader
      :prefix "m"
      :desc "Apply GTD context" "t" 'my/set-agenda-filter)

;; INITIALISE ORG-----------------------------------------------------
(after! org
  ;; - MISC -
  org-agenda-file-regexp "^.*\\.org$"

  ;; - TAGS -
  ;; These are selectable via (org-set-tags-command) or (counsel-org-tag).
  (setq org-tag-alist
        '((:startgroup)
          ;; Put mutually exclusive tags here.
          (:endgroup)
          ("@errand" . ?e)
          ("@laptop" . ?l)
          ("@dev" . ?d)))

  ;; - PRIORITIES -
  (setq org-highest-priority 65
        org-lowest-priority 69
        org-default-priority 68
        org-priority-faces '((65 :foreground "red" :weight bold)
                             (66 :foreground "orange" :weight bold)
                             (67 :foreground "yellow" :weight bold)
                             (68 :foreground "green" :weight bold)
                             (69 :foreground "#2a7286" :weight bold)))

  ;; - CAPTURE TEMPLATES -
  (setq org-capture-templates
        (append org-capture-templates ;; Don't overwrite.
                '(("i" "Inbox Item (GTD)" entry
                   (file "inbox.org")
                   "* %?")))))
