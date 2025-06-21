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
(setq gtd-dir (f-join (file-truename org-roam-directory) "gtd")
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

(defun gtd--slugify (fname)
  "xfs-slugify, but with downcase."
  (downcase (xfs-slugify fname)))

(defun gtd--org-fname (fname)
  "Create a slugified FNAME with a .org suffix."
  (if (string= (file-name-extension fname) "org")
      (gtd--slugify fname)
    (concat (gtd--slugify fname) ".org")))

(defun gtd-project-files()
  "Return a list of GTD project files."
  (directory-files gtd-projects-dir nil "^[^.].*")) ; Exclude dotfiles.

(defun gtd--filter-project-buckets (comp-candidate)
  "Filter for project nodes."
  (string-prefix-p gtd-projects-dir
                   (org-roam-node-file (cdr comp-candidate)))) ; Must get the cdr of the completion candidate first.

(defun gtd--find-project-bucket (&optional require-match)
  "Pick a project bucket, and return a roam node.
REQUIRE-MATCH: if nil, the return result may contain a nil node--a node whose fields are nil.
For example, '(org-roam-node-file node)' (the file path) will be nil. Setting this option to
nil is useful in scenarios where you want to create the node, if it doesn't exist."
  (interactive)
  (org-roam-node-read nil #'gtd--filter-project-buckets nil require-match "Pick a project: "))

(defun gtd-open-project ()
  "Find and open a project file. Create one if it doesn't exist."
  (interactive)
  (org-roam-node-find nil nil #'gtd--filter-project-buckets))

(defun gtd--project-create (title)
  "Create a GTD project.
This creates a new org-roam project under the GTD projects directory.
\nReturns the full path."
  (interactive "MEnter a project title: ")
  (xroam-node-create-at-path (f-join gtd-projects-dir (gtd--org-fname title)) title))

(defvar gtd--last-picked-project nil "This tracks the last project selected via 'gtd-refile-to-project'. It's used to repopulate initial input between usages.")
(defun gtd-refile-to-project ()
  "Pick a project to refile to; create one if it doesn't exist.
RETURN: It may return nil in cases where cancellation occurs,
otherwise it returns the full path to the selected node."
  (interactive)
  (let* ((roam-node (gtd--find-project-bucket))
         (node-path (org-roam-node-file roam-node)) ; Will be nil if node doesn't exist.
         (node-title (org-roam-node-title roam-node)) ; Node may not exist, but we provided a title upon creation.
         (node-path (if node-path node-path (gtd--project-create node-title)))) ; Prompt to create a file if we don't have one.
    (when node-path ; Something may go wrong (cancellation etc.), so must be non-nil.
      (org-refile nil nil (list nil node-path)))
    node-path)) ; May return nil.

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

(defun gtd--find-by-path (path)
  (interactive)
  (xvulpea-node-find nil nil
                     (lambda (n)
                       (string-match-p path (org-roam-node-file n)))))

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
       :n "P" #'gtd-project-create
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
