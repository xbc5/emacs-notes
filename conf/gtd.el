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


;; CONFIG VARS -------------------------------------------------------
;; - BUCKET PATHS -
(setq gtd-dir (f-join org-roam-directory "gtd")
      gtd-active-dir (f-join gtd-dir "active")
      gtd-dormant-dir (f-join gtd-dir "dormant")
      gtd-inactive-dir (f-join gtd-dir "inactive")
      gtd-projects-dir (f-join gtd-active-dir "projects")
      gtd-inbox-fpath (f-join gtd-active-dir "inbox.org")
      gtd-tasks-fpath (f-join gtd-active-dir "tasks.org")
      gtd-tickler-fpath (f-join gtd-inactive-dir "tickler.org")
      gtd-trash-fpath (f-join gtd-inactive-dir "trash.org")
      gtd-read-later-fpath (f-join gtd-inactive-dir "read_later.org")
      gtd-someday-or-maybe-fpath (f-join gtd-inactive-dir "someday_or_maybe.org"))


;; UTILS -------------------------------------------------------------
(defun gtd-set-active-candidates ()
  "Set the 'org-agenda-files' actionable, or semi-actionable task files.
Essentially, determine active and dormant file paths, then set 'org-agenda-files'.\n
Active files:   Contains actionable tasks that should be displayed,
                 e.g., tasks, and projects.
Dormant files:  Contains tasks that become active at a set time,
                 e.g., items in the tickler file."
  (setq org-agenda-files (directory-files gtd-active-dir t "\\.org$")))

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


;; PRE-INITIALISATION ------------------------------------------------
;; - DIRECTORY CREATION -
(make-directory gtd-active-dir t)
(make-directory gtd-inactive-dir t)

;; - DEFAULT BUCKETS CREATION -
(after! org-roam
  ;; These rely on roam functions to register IDs.
  (xroam-node-create-at-path gtd-inbox-fpath "inbox for GTD")
  (xroam-node-create-at-path gtd-tasks-fpath "tasks for GTD")
  (xroam-node-create-at-path gtd-someday-or-maybe-fpath "someday or maybe for GTD")
  (xroam-node-create-at-path gtd-trash-fpath "trash for GTD")
  (xroam-node-create-at-path gtd-read-later-fpath "read later for GTD")
  (xroam-node-create-at-path gtd-tickler-fpath "tickler for GTD"))

(map! :leader
      :prefix "m"
      :desc "Apply GTD context" "t" 'my/set-agenda-filter)


;; INITIALISE ORG ----------------------------------------------------
(after! org
  ;; - MISC -
  org-agenda-file-regexp "^.*\\.org$"

  ;; - FILE PATHS -
  ;; Reset the agenda files (e.g., after creating a new project.)
  (add-hook 'org-capture-after-finalize-hook #'gtd-set-active-candidates)
  (gtd-set-active-candidates) ; Init agenda files.
  (add-hook 'org-capture-after-finalize-hook #'gtd-set-dormant-files)
  (gtd-set-active-candidates) ; Init agenda files.
  ;; Reset the refile targets (e.g., after creating a new project.)
  (add-hook 'org-capture-after-finalize-hook #'gtd-set-refile-targets)
  (gtd-set-refile-targets) ; Init refile targets.

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
  (setq org-highest-priority 65
        org-lowest-priority 69
        org-default-priority 68
        org-priority-faces '((65 :foreground "red" :weight bold)
                             (66 :foreground "orange" :weight bold)
                             (67 :foreground "yellow" :weight bold)
                             (68 :foreground "green" :weight bold)
                             (69 :foreground "#2a7286" :weight bold)))

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
