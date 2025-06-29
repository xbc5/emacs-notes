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
;; - Tasks: If none of the above, it goes here. Most items end up here, and they should be actionable.
;;

;; For "Next Action" and "Waiting For" states, use the Org task states: NEXT or WAIT.
;;
;; For priorities, use Org priorities.
;;
(require 'f)
(require 'filenotify) ; Required for 'file-notify-add-watch'.

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
      gtd-someday-or-maybe-fpath (f-join gtd-inactive-dir "someday_or_maybe.org") ; Invisible
      gtd-trash-fpath (f-join gtd-inactive-dir "trash.org") ; Invisible
      ;; - SETTINGS -
      gtd-context-tags-fpath (f-join gtd-dir "context_tags.txt"))

;; UTILS -------------------------------------------------------------
(defun gtd-file-tasks-open () (interactive) (find-file gtd-tasks-fpath))
(defun gtd-file-tickler-open () (interactive) (find-file gtd-tickler-fpath))
(defun gtd-file-inbox-open () (interactive) (find-file gtd-inbox-fpath))
(defun gtd-file-someday-or-maybe-open () (interactive) (find-file gtd-someday-or-maybe-fpath))
(defun gtd-file-trash-open () (interactive) (find-file gtd-trash-fpath))

(defun gtd--buckets-active-get ()
  "Get all active buckets."
  ;; Emacs Regex is stupid, hard to read, and it doesn't work, so do two recursive searches instead.
  (append
   (directory-files-recursively gtd-active-dir "\\.org$")
   (directory-files-recursively gtd-dormant-dir "\\.org$")))

(defun gtd--buckets-inactive-get ()
  "Get all inactive buckets."
  (directory-files-recursively gtd-inactive-dir "\\.org$"))

(defun gtd--buckets-all-get ()
  "Get all buckets, active and inactive."
  (append (gtd--buckets-active-get) (gtd--buckets-inactive-get)))

(defun gtd-set-active-candidates ()
  "Set the 'org-agenda-files' actionable, or semi-actionable task files.
Essentially, determine active and dormant file paths, then set 'org-agenda-files'.\n
Active files:   Contains actionable tasks that should be displayed,
                 e.g., tasks, and projects.
Dormant files:  Contains tasks that become active at a set time,
                 e.g., items in the tickler file."
  ;; Emacs Regex is stupid, hard to read, and it doesn't work, so do two recursive searches instead.
  (setq org-agenda-files (gtd--buckets-active-get)))

(defun gtd--refile-targets-get ()
  "Get all GTD refile targets. This is every org file suitable for targeting."
  ;; Set it to the 'gtd-dir', because we want to refile to both inactive and active.
  '((gtd--buckets-active-get :maxlevel 10)
    (gtd--buckets-inactive-get :maxlevel 10)))

(defun gtd--refile-targets-set ()
  "Set the 'org-refile-targets' to all org files under the GTD active, dormant, and inactive directories.
\nUse this upon init, and after creating files in the GTD /active/ directory (e.g., via a hook.)"
  ;; Set it to the 'gtd-dir', because we want to refile to both inactive and active.
  (setq org-refile-targets (gtd--refile-targets-get)))

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


;; FILTERS -----------------------------------------------------------
;; These filter Org-Roam completion menu. Often we want to target a set of specific
;; buckets, for example, sub-nodes of the tasks file, or sub-projects. These filter
;; by file path.
;;
;; - PROJECT FILTER -
(defun gtd--bucket-filter-project (comp-candidate)
  "Filter completion candidates for project nodes."
  (string-prefix-p gtd-projects-dir
                   (org-roam-node-file (cdr comp-candidate)))) ; Must get the cdr of the completion candidate first.

;; - SOMEDAY OR MAYBE FILTER -
(defun gtd--bucket-filter-someday-or-maybe (comp-candidate)
  "Filter completion candidates for someday or maybe nodes."
  (string= gtd-someday-or-maybe-fpath (org-roam-node-file (cdr comp-candidate))))

;; - TASKS FILTER -
(defun gtd--bucket-filter-tasks (comp-candidate)
  "Filter completion candidates for task nodes."
  (string= gtd-tasks-fpath (org-roam-node-file (cdr comp-candidate))))

;; - TICKLER FILTER -
;; This is a WIP. I have not yet determined what the nodes in a tickler file will look like.
;; Perhaps they will be a date tree, and perhaps these will be Roam nodes--I don't know.
(defun gtd--bucket-filter-tickler (comp-candidate)
  "Filter completion candidates for tickler nodes."
  (string= gtd-tickler-fpath (org-roam-node-file (cdr comp-candidate))))


;; -------------------------------------------------------------------
(defun gtd--bucket-project-find (&optional initial-input require-match)
  "Pick a project bucket, and return a roam node.
INITIAL-INPUT: Like roam-find, this is the value entered into the input section upon opening the window.
\nREQUIRE-MATCH: if nil, the return result may contain a nil node--a node whose fields are nil.
For example, '(org-roam-node-file node)' (the file path) will be nil. Setting this option to
nil is useful in scenarios where you want to create the node, if it doesn't exist."
  (interactive)
  (org-roam-node-read initial-input #'gtd--bucket-filter-project nil require-match "Pick a project: "))

(defun gtd-file-project-open ()
  "Find and open a project file. Create one if it doesn't exist."
  (interactive)
  (org-roam-node-find nil nil #'gtd--bucket-filter-project))

(defun gtd--project-create (title)
  "Create a GTD project.
This creates a new org-roam project file under the GTD projects directory.
\nReturns the full path."
  (interactive "MEnter a project title: ")
  (xroam-node-create-at-path (f-join gtd-projects-dir (gtd--org-fname title)) title))


;; REFILERS ----------------------------------------------------------
;; - GENERIC REFILER -
;; This does the actual refiling for the below functions.
(defun gtd--refile (filter-fn prompt &optional initial-input)
  "Refile the current node to any target node defined by FILTER-FN."
  ;; Pick the node first, so that it doesn't apply TODO and priority while we're picking.
  (let* ((roam-node (org-roam-node-read initial-input filter-fn nil t prompt)))
    ;; Apply TODO if not already set.
    (unless (org-get-todo-state)
      (org-todo "TODO"))

    ;; Apply priority 3 if not already set.
    (unless (> (org-get-priority (org-get-heading)) 0)
      (org-priority org-default-priority))

    ;; Since we hover over nodes to refile, this will create a Roam node
    ;; if it's not already a Roam node; it does nothing otherwise.
    (org-id-get-create)

    ;; Refile,
    (org-roam-refile roam-node)
    (org-save-all-org-buffers)))

;; - PROJECT REFILER -
;; List all projects, and sub-projects, pick one, and refile to it.
(defun gtd-refile-to-project ()
  "Pick an existing project to refile to."
  (interactive)
  (gtd--refile #'gtd--bucket-filter-project "Refile to project: "))

;; - TASKS REFILER -
;; Tasks may also have subgroupings, just for organisational purposes.
(defun gtd-refile-to-tasks ()
  "Refile to the tasks file."
  (interactive)
  (gtd--refile #'gtd--bucket-filter-tasks "Refile to tasks: "))

;; - SOMEDAY OR MAYBE REFILER -
;; The Someday or maybe file will also have organisational subgroups.
(defun gtd-refile-to-someday-or-maybe ()
  "Refile to the someday or maybe file."
  (interactive)
  (gtd--refile #'gtd--bucket-filter-someday-or-maybe "Refile to someday or maybe: "))

;; - TICKLER REFILER -
;; I may want to refile to an existing date node, for example.
(defun gtd-refile-to-tickler ()
  "Refile to the tickler file."
  (interactive)
  (gtd--refile #'gtd--bucket-filter-tickler "Refile to someday or maybe: "))

;; - TRASH REFILER -
;; The trash is flat.
(defun gtd-refile-to-trash () (interactive) (org-refile nil nil (list nil gtd-trash-fpath)))


;; -------------------------------------------------------------------
(defun my/org-find-headline-position (headline)
  "Return the position of HEADLINE in FILE."
  (marker-position
   (org-find-exact-headline-in-buffer headline)))

(defun gtd--find-by-path (path)
  (interactive)
  (xvulpea-node-find nil nil
                     (lambda (n)
                       (string-match-p path (org-roam-node-file n)))))


;; TAG UTILS ---------------------------------------------------------
;; - TAG CACHE -
(defvar gtd--tag-filter-candidates nil "A stash for tags that may become ")

(defun gtd--tag-buf-valid-p (buf)
  "Validate that the tag buffer/file is valid.
It tests that each line is in the form @tag:k"
  (with-current-buffer buf
    (cl-every
     (lambda (line)
       (let ((tag (car line))
             (key (cdr line)))
         (and (stringp tag)
              (length> tag 1) ; Tag mst be at least '@x'
              (string-prefix-p "@" tag)
              (stringp key)
              (length= key 1) ; Key is a, b, x, y, ...
              (string-match-p "^[[:graph:]]$" key)))) ; Is any printable char, except space.
     (gtd--tags-string-to-alist (buffer-string)))))

(defun gtd-tag-file-edit ()
  "Edit the tag file in a popup buffer."
  (interactive)
  (my/doom-popup-buffer gtd-context-tags-fpath nil #'gtd--tag-buf-valid-p))

;; - TAG TOGGLER -
(defun gtd--toggle-tag (tag)
  "Toggle tags on and off, for the tag filter."
  (let* ((pattern (format "^[+-]%s$" tag)))
    ;; If it's set, remove it.
    (if
        ;; Check for tag.
        (cl-some (lambda (tg) (string-match-p pattern tg))
                 gtd--tag-filter-candidates)
        ;; Remove tag if found.
        (setq gtd--tag-filter-candidates
              (cl-remove-if
               (lambda (tg) (string-match-p pattern tg))
               gtd--tag-filter-candidates))

      ;; Else, add the tag.
      (push (concat "+" tag) gtd--tag-filter-candidates))
    (sort gtd--tag-filter-candidates #'string<)
    (message (mapconcat 'identity gtd--tag-filter-candidates "")) ; See the tags as we modify them.
    gtd--tag-filter-candidates))

;; - GLOBAL TAG STATE MANAGERS -
(defun gtd--reset-tag-candidates ()
  "Reset 'gtd--tag-filter-candidates'"
  (setq gtd--tag-filter-candidates nil))

(defun gtd--use-tag-candidates ()
  "Set 'org-agenda-tag-filter-preset' with 'gtd--tag-filter-candidates'
Why? Because we don't modify the agenda list directly."
  (setq org-agenda-tag-filter-preset gtd--tag-filter-candidates))

;; - TAG MENU -
(defun gtd--refresh-tag-menu (&optional context-tags)
  "Build a hydra menu from the contents of 'gtd--context-tags-menu'.
This is the menu you use to filter tags in the agenda view.
\nCONTEXT-TAGS is an alist of (tag . key) pairs. If you do not provide this,
it defaults to 'gtd--context-tags' (the global)."
  (let ((heads (append
                ;; - STATIC HEADS -
                '(("q"  (progn (gtd--use-tag-candidates)
                               (gtd--reset-tag-candidates)) "Quit" :exit t)
                  ("M-c" (progn (gtd--reset-tag-candidates) nil) "Cancel" :exit t))
                ;; - DYNAMIC HEADS -
                (mapcar (lambda (tag-key)
                          (let ((tag (car tag-key))
                                (key (cdr tag-key)))
                            (list key `(gtd--toggle-tag ,tag) tag)))
                        (or context-tags gtd--context-tags)))))
    ;; - APPLY THE HEADS -
    (eval `(defhydra
             gtd-toggle-tags
             (:foreign-keys run ; Prevent Hydra from closing when non-hydra key pressed.
              :on-enter
              (setq gtd--tag-filter-candidates ; Work upon a copy of applied tags.
                    org-agenda-tag-filter-preset)) "Tags" ,@heads))))

;; TAGS --------------------------------------------------------------
(defvar gtd--context-tags nil "A list of (\"@<tag>\" . \"<key>\") pairs.")

;; - CONTEXT TAG LOADER -
(defun gtd--load-context-tags ()
  "Load context tags from a file and set a global variable."
  (with-temp-buffer
    (insert-file-contents gtd-context-tags-fpath)
    (setq gtd--context-tags
          (gtd--tags-string-to-alist (buffer-string)))))

;; - ORG TAG GENERATOR -
(defun gtd--generate-org-tags (&optional tags-alist)
  "Produce org tags from the global tags variable."
  (mapcar
   (lambda (con)
     (let* ((tag (car con))
            (key (string-to-char (cdr con)))) ; We need a keycode for the key.
       (cons tag key)))
   (or tags-alist gtd--context-tags)))

;; - TAG LOADERS -
;; A convenience method to run both tag loaders.
(defun gtd--set-tag-variables ()
  "Set all of the tag variables. Run this after the tag file changes."
  (gtd--load-context-tags)
  (setq org-tag-alist (gtd--generate-org-tags)))

;; - ORG REFRESHER -
;; Tag change do not reflect in org-mode, so we need to
;; manually refresh each open Org file.
(defun gtd--reparse-org-tag-properties ()
  "Refresh Org tag configuration in all open Org buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'org-mode)
        ;; Reparse PROPERTIES for tag specific properties only.
        (org-set-regexps-and-options t)))))

;; - TAG OBSERVER -
(defun gtd--tag-observer (symbol newval operation where)
  "Refresh depencencies when GTD context tags change."
  (when (eq operation 'set)
    (setq org-tag-alist (gtd--generate-org-tags newval))
    (gtd--refresh-tag-menu newval)
    (gtd--reparse-org-tag-properties)))
;; Actually watch for changes to tags.
(add-variable-watcher 'gtd--context-tags #'gtd--tag-observer)

;; - AGENDA TAG REFRESHER -
(defun gtd--agenda-tag-preset-watcher (symbol newval operation where)
  "Refresh the agenda view when the user applies new tags."
  (when (and (eq operation 'set) (get-buffer "*Org Agenda*"))
    (with-current-buffer "*Org Agenda*"
      (run-at-time "0.01 sec" nil ; It's a little racy; we need to delay.
                   (lambda () (org-agenda-redo))))))
;; Observe for tag changes, then refresh.
(add-variable-watcher 'org-agenda-tag-filter-preset #'gtd--agenda-tag-preset-watcher)

(defun gtd--tags-string-to-alist (tags-str)
  "Take a multi-line string of tag:key, and return an alist.
The tag buffer contains such file contents, and this maps the
buffer into an alist."
  (sort
   (mapcar (lambda (line)
             (let* ((parts (split-string line ":"))
                    (tag (car parts))
                    (key (cadr parts)))
               (cons tag key)))
           (split-string tags-str "\n" t))))


(defun gtd-next-actions ()
  "Display next actions."
  (interactive)
  ;;(org-todo-list "NEXT")
  (let* ((org-super-agenda-groups
          '((:name "Next Actions"
             :todo "NEXT")
            (:name "Waiting"
             :todo "WAIT")
            (:discard (:todo "TODO")) ; We don't want to see TODO items in this view.

            )))
    (org-agenda nil "t"))

  )

(defun gtd-daily-review ()
  "Display ALL items from active buckets, grouped by file."
  (interactive)
  (let* ((org-super-agenda-groups
          '((:name "Daily Review"
             :auto-category t))))
    (org-agenda nil "t")))

(defun gtd-weekly-review ()
  "Display ALL items from ALL buckets, grouped by file."
  (interactive)
  (let* ((org-agenda-files (gtd--buckets-all-get))
         (org-super-agenda-groups
          '((:name "Weekly Review"
             :auto-category t))))
    (org-agenda nil "t")))

;; PRE-INITIALISATION ------------------------------------------------
;; - DIRECTORY CREATION -
(make-directory gtd-active-dir t)
(make-directory gtd-dormant-dir t)
(make-directory gtd-inactive-dir t)

;; - LOAD TAGS -
(xtouch-new gtd-context-tags-fpath) ; We want a tag file to exist.

;; Load the tags from the file into a global. This should trigger
;; an observer, which will refresh the org-tag-alist, and the hydra menu.
(gtd--load-context-tags)
(gtd--refresh-tag-menu)

;; Watch the tags file. Update the global upon change.
(when (version<= "24.4" emacs-version)
  (file-notify-add-watch ; Must (require 'filenotify)
   gtd-context-tags-fpath
   '(change)
   (lambda (event)
     (when (eq (cadr event) 'changed)
       (gtd--set-tag-variables)))))


;; - ORG AGENDA FILES -
;; Do this outside of after!, because it works here, but not there.
(gtd-set-active-candidates)
(gtd--refile-targets-set)

;; - DEFAULT BUCKETS CREATION -
(after! org-roam
  ;; Because we create Roam (not Org) nodes we need to hook Roam.
  ;; See 'gtd-project-create' for example, it uses 'xroam-node-create-at-path'.
  (add-hook 'org-roam-capture-new-node-hook #'gtd-set-active-candidates) ; Update Org agenda files.
  (add-hook 'org-roam-capture-new-node-hook #'gtd--refile-targets-set) ; We may want to refile to the new files.
  (add-hook 'org-agenda-mode-hook #'org-super-agenda-mode) ; We need to activate super agenda too.

  ;; These rely on roam functions to register IDs.
  (xroam-node-create-at-path gtd-inbox-fpath "inbox for GTD")
  (xroam-node-create-at-path gtd-tasks-fpath "tasks for GTD")
  (xroam-node-create-at-path gtd-someday-or-maybe-fpath "someday or maybe for GTD")
  (xroam-node-create-at-path gtd-trash-fpath "trash for GTD")
  (xroam-node-create-at-path gtd-tickler-fpath "tickler for GTD"))


;; KEYMAPS -----------------------------------------------------------
(global-unset-key (kbd "M-t"))

;; - NORMAL KEYMAPS -
(map! :leader
      (:prefix "j"
       :n "P" #'gtd-project-create
       :n "T" #'gtd-tag-file-edit
       (:prefix "r" ; Refile and review.
        :desc "Refile to Project" :n "p" #'gtd-refile-to-project
        :desc "Refile to Someday or Maybe" :n "s" #'gtd-refile-to-someday-or-maybe
        :desc "Refile to Tasks" :n "t" #'gtd-refile-to-tasks
        :desc "Refile to Tickler" :n "k" #'gtd-refile-to-tickler
        :desc "Refile to Trash" :n "x" #'gtd-refile-to-trash
        :desc "Review Daily" :n "d" #'gtd-daily-review
        :desc "Review Weekly" :n "w" #'gtd-weekly-review)
       (:prefix "o" ; Open.
        :desc "Create Project" :n "P" #'gtd--project-create
        :desc "Inbox" :n "i" #'gtd-file-inbox-open
        :desc "Project" :n "p" #'gtd-file-project-open
        :desc "Someday or Maybe" :n "s" #'gtd-file-someday-or-maybe-open
        :desc "Tasks" :n "t" #'gtd-file-tasks-open
        :desc "Tickler" :n "k" #'gtd-file-tickler-open
        :desc "Trash" :n "x" #'gtd-file-trash-open)))

;; - ORG AGENDA KEYMAPS -
(map! :after org-agenda
      "M-t" #'gtd-next-actions)

(after! evil-org-agenda
  ;; Unmap/remap the useless motion keys.
  ;; NOTE: that Org agenda is ALWAYS in 'motion state;
  ;; mapping to other states does nothing.
  (evil-define-key 'motion evil-org-agenda-mode-map
    "l" #'undefined
    "h" #'undefined
    "w" #'undefined
    "W" #'undefined
    "b" #'undefined
    "B" #'undefined
    "e" #'gtd-toggle-tags/body
    "E" #'undefined
    "f" #'undefined
    "F" #'undefined
    "(" #'undefined
    ")" #'undefined
    "^" #'undefined
    "_" #'undefined
    "`" #'undefined
    "," #'undefined
    "'" #'undefined
    "?" #'undefined
    "#" #'undefined
    ";" #'undefined
    "$" #'undefined))

;; - ORG KEYMAPS -
;; Doom swaps 'org-set-tags-command' with 'counsel-org-tag'[0]:
;; > [remap org-set-tags-command]     #'counsel-org-tag
;; We want to undo that.
;; [0] https://github.com/doomemacs/doomemacs/blob/e6c755305358412a71a990fc2cf592c629edde1e/modules/completion/ivy/config.el#L178
(after! (:and counsel org)
  ;; Prevent counsel-mode from overriding 'org-set-tags-command'.
  (define-key counsel-mode-map [remap org-set-tags-command] nil)
  ;; Set 'org-set-tags-command' back to itself.
  (define-key org-mode-map [remap org-set-tags-command] #'org-set-tags-command)
  ;; Now map M-t.
  (map! :map org-mode-map "M-t" #'org-set-tags-command))


;; INITIALISE ORG ----------------------------------------------------
(after! org
  ;; - MISC -
  org-agenda-file-regexp "^.*\\.org$"

  ;; This option makes refile targets paths when picking
  ;; them using ido, or other fuzzy selector.
  ;; Source: https://stackoverflow.com/a/21335010
  (setq org-refile-use-outline-path 'file)

  ;; Use fuzzy like completions (where applicable). t means choose it in steps (annoying).
  (setq org-outline-path-complete-in-steps nil)

  (setq org-refile-allow-creating-parent-nodes t)

  ;; Refile to the top of the target file.
  (setq org-reverse-note-order t)

  ;; - TAGS -
  ;; These are selectable via (org-set-tags-command) or (counsel-org-tag).
  (setq org-tag-alist (gtd--generate-org-tags))

  ;; - TASK PRIORITIES -
  (setq org-highest-priority ?A
        org-lowest-priority ?C
        org-default-priority ?C
        org-priority-faces '((?A :foreground "red" :weight bold)
                             (?B :foreground "yellow" :weight bold)
                             (?C :foreground "green" :weight bold)))

  (setq org-agenda-custom-commands
        '(("n" "Next Actions" todo "NEXT")
          ("i" "Inbox" tags "+LEVEL>0"
           ((org-agenda-files  (list gtd-inbox-fpath))))))

  ;; - TASK STATES -
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)" "DROP(c)"))
        org-todo-keyword-faces
        '(("NEXT" :foreground "magenta")
          ("TODO" :foreground "#16cafa")
          ("WAIT" :foreground "brown")
          ("DONE" :foreground "#666666")
          ("DROP" :foreground "#666666")))

  ;; - CAPTURE TEMPLATES -
  (setq org-capture-templates
        '(("i" "Inbox Item (GTD)" entry
           (file gtd-inbox-fpath)
           "* %?"))))
