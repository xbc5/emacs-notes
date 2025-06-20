;; -*- lexical-binding: t; -*-
;;
;; The Getting Things Done method, implemented via Org Agenda.
;;
;; The bucket paths are split into 'active' and 'inactive.' The 'active' paths
;; are visible via Org agenda; the 'inactive' files are not.
;;
;; Buckets:
;; - Inbox: A dumping ground for unclarified items.
;; - Projects: A set of named files, each containing a set of project tasks.
;; - Ticker: A single file with a set of date headers. A reminder is set when they're due.
;; - Trash: Where everything eventually goes.
;; - Someday/Maybe: Things that I want to do, but don't know what or when.
;; - Read Later: Like Someday/Maybe, except for books and articles.
;; - Tasks: If none of the above, it goes here. Most items end up here, and they should be actionable.
;;

;; For "Next Action" and "Waiting For" states, use the Org task states: NEXT or WAIT.
;;
;; For priorities, use Org priorities.
;;
(require 'f)

;; TODO:
;; - Shortcuts to open each file directly.
;; - Refile to a specific file

;; CONFIG VARS -------------------------------------------------------
;; - BUCKET PATHS -
(setq gtd-dir (f-join org-roam-directory "gtd")
      ;; - DIRS -
      gtd-active-dir (f-join gtd-dir "active")
      gtd-dormant-dir (f-join gtd-dir "dormant")
      gtd-inactive-dir (f-join gtd-dir "inactive")
      gtd-projects-dir (f-join gtd-active-dir "projects")
      ;; - FPATHS -
      gtd-tasks-fpath (f-join gtd-active-dir "tasks.org") ; Visible.
      gtd-tickler-fpath (f-join gtd-dormant-dir "tickler.org") ; Semi-visible.
      gtd-inbox-fpath (f-join gtd-inactive-dir "inbox.org") ; Invisible
      gtd-read-later-fpath (f-join gtd-inactive-dir "read_later.org") ; Invisible
      gtd-someday-or-maybe-fpath (f-join gtd-inactive-dir "someday_or_maybe.org") ; Invisible
      gtd-trash-fpath (f-join gtd-inactive-dir "trash.org")) ; Invisible

;; UTILS -------------------------------------------------------------
(defun gtd-set-active-candidates ()
  "Set the 'org-agenda-files' actionable, or semi-actionable task files.
Essentially, determine active and dormant file paths, then set 'org-agenda-files'.\n
Active files:   Contains actionable tasks that should be displayed,
                 e.g., tasks, and projects.
Dormant files:  Contains tasks that become active at a set time,
                 e.g., items in the tickler file."
  ;; Emacs Regex is stupid, hard to read, and it doesn't work, so do two recursive searches instead.
  (setq org-agenda-files (append
                          (directory-files-recursively gtd-active-dir "\\.org$")
                          (directory-files-recursively gtd-dormant-dir "\\.org$"))))

(defun gtd-set-refile-targets ()
  "Set the 'org-refile-targets' to all org files under the GTD directory.
\nUse this upon init, and after creating files in the GTD /active/ directory (e.g., via a hook.)"
  ;; Set it to the 'gtd-dir', because we want to refile to both inactive and active.
  (setq org-refile-targets (directory-files-recursively gtd-dir "\\.org$")))

(defun gtd-project-create (title)
  "Create a GTD project.
\nThis creates a new org-roam node under the GTD projects directory."
  (interactive "MEnter a project title: ")
  (xroam-node-create-at-path (f-join gtd-projects-dir
                                     (concat (xfs-slugify title) ".org")) ; fname: e.g., foo_bar.org
                             title))

(defun my/org-find-headline-position (headline)
  "Return the position of HEADLINE in FILE."
  (marker-position
   (org-find-exact-headline-in-buffer headline)))

(defun my/org-ensure-todo-state ()
  "Ensure that the current org node has a TODO state set."
  (unless (org-get-todo-state)
    (org-todo "TODO")))

(defun my/org-ensure-priority ()
  "Ensure that the current org node has a TODO state set."
  (unless (> (org-get-priority (org-get-heading)) 0)
    (org-priority org-default-priority)))

;; - REFILERS -
;; These refile to the root node in target paths.
(defun gtd-refile-to-tasks () (interactive) (org-refile nil nil (list nil gtd-tasks-fpath)))
(defun gtd-refile-to-tickler () (interactive) (org-refile nil nil (list nil gtd-tickler-fpath)))
(defun gtd-refile-to-read-later () (interactive) (org-refile nil nil (list nil gtd-read-later-fpath)))
(defun gtd-refile-to-someday-or-maybe () (interactive) (org-refile nil nil (list nil gtd-someday-or-maybe-fpath)))
(defun gtd-refile-to-trash () (interactive) (org-refile nil nil (list nil gtd-trash-fpath)))


;; PRE-INITIALISATION ------------------------------------------------
;; - DIRECTORY CREATION -
(make-directory gtd-active-dir t)
(make-directory gtd-dormant-dir t)
(make-directory gtd-inactive-dir t)

;; - ORG AGENDA FILES -
;; Do this outside of after!, because it works here, but not there.
(gtd-set-active-candidates)
(gtd-set-refile-targets)

;; - DEFAULT BUCKETS CREATION -
(after! org-roam
  ;; Because we create Roam (not Org) nodes we need to hook Roam.
  ;; See 'gtd-project-create' for example, it uses 'xroam-node-create-at-path'.
  (add-hook 'org-roam-capture-new-node-hook #'gtd-set-active-candidates) ; Update Org agenda files.
  (add-hook 'org-roam-capture-new-node-hook #'gtd-set-refile-targets) ; We may want to refile to the new files.

  ;; These rely on roam functions to register IDs.
  (xroam-node-create-at-path gtd-inbox-fpath "inbox for GTD")
  (xroam-node-create-at-path gtd-tasks-fpath "tasks for GTD")
  (xroam-node-create-at-path gtd-someday-or-maybe-fpath "someday or maybe for GTD")
  (xroam-node-create-at-path gtd-trash-fpath "trash for GTD")
  (xroam-node-create-at-path gtd-read-later-fpath "read later for GTD")
  (xroam-node-create-at-path gtd-tickler-fpath "tickler for GTD"))

;; - KEYMAPS -
(map! :leader
      (:prefix "j"
       :n "r" #'gtd-refile-to-read-later
       :n "s" #'gtd-refile-to-someday-or-maybe
       :n "t" #'gtd-refile-to-tasks
       :n "k" #'gtd-refile-to-tickler
       :n "x" #'gtd-refile-to-trash))


;; INITIALISE ORG ----------------------------------------------------
(after! org
  ;; - MODS -
  ;; We want Org nodes to have a priority of 3 by default.
  (advice-add 'org-refile :before (lambda (&rest _) (my/org-ensure-priority)))
  ;; We want Org nodes to at least have a TODO state before refiling.
  (advice-add 'org-refile :before (lambda (&rest _) (my/org-ensure-todo-state)))
  ;; We don't want unsaved buffers after refiling.
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

  ;; - MISC -
  org-agenda-file-regexp "^.*\\.org$"

  ;; This option makes refile targets paths when picking
  ;; them using ido, or other fuzzy selector.
  ;; Source: https://stackoverflow.com/a/21335010
  (setq org-refile-use-outline-path 'file)

  ;; Use fuzzy like completions (where applicable). t means choose it in steps (annoying).
  (setq org-outline-path-complete-in-steps nil)

  ;; Refile to the top of the target file.
  (setq org-reverse-note-order t)

  ;; - TAGS -
  ;; These are selectable via (org-set-tags-command) or (counsel-org-tag).
  (setq org-tag-alist
        '((:startgroup)
          ;; Put mutually exclusive tags here.
          (:endgroup)
          ("@errand" . ?e)
          ("@laptop" . ?l)
          ("@dev" . ?d)))

  ;; - TASK PRIORITIES -
  (setq org-highest-priority ?A
        org-lowest-priority ?C
        org-default-priority ?C
        org-priority-faces '((?A :foreground "red" :weight bold)
                             (?B :foreground "yellow" :weight bold)
                             (?C :foreground "green" :weight bold)))

  ;; - TASK STATES -
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)" "DROP(c)"))
        org-todo-keyword-faces
        '(("TODO" :foreground "#16cafa")
          ("NEXT" :foreground "magenta")
          ("WAIT" :foreground "brown")
          ("DONE" :foreground "#666666")
          ("DROP" :foreground "#666666")))

  ;; - CAPTURE TEMPLATES -
  (setq org-capture-templates
        '(("i" "Inbox Item (GTD)" entry
           (file gtd-inbox-fpath)
           "* %?"))))
