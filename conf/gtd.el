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
      gtd-read-later-fpath (f-join gtd-inactive-dir "read_later.org") ; Invisible
      gtd-someday-or-maybe-fpath (f-join gtd-inactive-dir "someday_or_maybe.org") ; Invisible
      gtd-trash-fpath (f-join gtd-inactive-dir "trash.org") ; Invisible
      ;; - SETTINGS -
      gtd-context-tags-fpath (f-join gtd-dir "context_tags.txt"))

;; UTILS -------------------------------------------------------------
(defun gtd-file-tasks-open () (interactive) (find-file gtd-tasks-fpath))
(defun gtd-file-tickler-open () (interactive) (find-file gtd-tickler-fpath))
(defun gtd-file-inbox-open () (interactive) (find-file gtd-inbox-fpath))
(defun gtd-file-read-later-open () (interactive) (find-file gtd-read-later-fpath))
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

(defun gtd--filter-project-buckets (comp-candidate)
  "Filter for project nodes."
  (string-prefix-p gtd-projects-dir
                   (org-roam-node-file (cdr comp-candidate)))) ; Must get the cdr of the completion candidate first.

(defun gtd--bucket-project-find (&optional initial-input require-match)
  "Pick a project bucket, and return a roam node.
INITIAL-INPUT: Like roam-find, this is the value entered into the input section upon opening the window.
\nREQUIRE-MATCH: if nil, the return result may contain a nil node--a node whose fields are nil.
For example, '(org-roam-node-file node)' (the file path) will be nil. Setting this option to
nil is useful in scenarios where you want to create the node, if it doesn't exist."
  (interactive)
  (org-roam-node-read initial-input #'gtd--filter-project-buckets nil require-match "Pick a project: "))

(defun gtd-file-project-open ()
  "Find and open a project file. Create one if it doesn't exist."
  (interactive)
  (org-roam-node-find nil nil #'gtd--filter-project-buckets))

(defun gtd--project-create (title)
  "Create a GTD project.
This creates a new org-roam project under the GTD projects directory.
\nReturns the full path."
  (interactive "MEnter a project title: ")
  (xroam-node-create-at-path (f-join gtd-projects-dir (gtd--org-fname title)) title))

(defun gtd--refile (filter-fn prompt &optional initial-input)
  "Refile the current node to any target node defined by FILTER-FN."
  (org-roam-refile
   (org-roam-node-read initial-input filter-fn nil t prompt)))

(defun gtd-refile-to-project ()
  "Pick an existing project to refile to."
  (interactive)
  (gtd--refile #'gtd--filter-project-buckets "Refile to project: "))

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
it defaults to 'gtd--context-tags-alist' (the global)."
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
                        (or context-tags gtd--context-tags-alist)))))
    ;; - APPLY THE HEADS -
    (eval `(defhydra
             gtd-toggle-tags
             (:foreign-keys run ; Prevent Hydra from closing when non-hydra key pressed.
              :on-enter
              (setq gtd--tag-filter-candidates ; Work upon a copy of applied tags.
                    org-agenda-tag-filter-preset)) "Tags" ,@heads))))

;; - OBSERVERS -
(defun gtd--refresh-tag-watcher (symbol newval operation where)
  "The handler that watches 'gtd--context-tags-alist' and refreshes the tag menu."
  (when (eq operation 'set)
    (gtd--refresh-tag-menu newval)
    (setq org-tag-alist (gtd--generate-org-tag-alist newval))
    (org-reload)))

(defun gtd--agenda-tag-preset-watcher (symbol newval operation where)
  "Handle the 'add-variable-watcher' for 'org-agenda-tag-filter-preset'.
Refresh the agenda view when new tags are applied."
  (when (and (eq operation 'set) (get-buffer "*Org Agenda*"))
    (with-current-buffer "*Org Agenda*"
      (run-at-time "0.01 sec" nil ; It's a little racy; we need to delay.
                   (lambda () (org-agenda-redo))))))

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

;; - TAG LOADER -
(defvar gtd--context-tags-alist nil "A list of (\"@<tag>\" . \"<key>\") pairs.")
(defun gtd--load-context-tags ()
  "Load the contents of the tag file into the context tag alist."
  (with-temp-buffer
    (insert-file-contents gtd-context-tags-fpath)
    (setq gtD--context-tags-alist
          (gtd--tags-string-to-alist (buffer-string)))))

(defun gtd--generate-org-tag-alist (&optional tags-alist)
  "Generate 'org-tag-alist' from 'gtd--context-tags-alist'
The GTD list is is a generic alist without character key codes. This function
processes that and turns it into a list suitable for use with org.
\nTAGS-ALIST: A value in the same shap of 'gtd--context-tags-alist'.
\nRETURN: An alist, suitable to set to 'org-tag-alist'."
  (mapcar
   (lambda (con)
     (let* ((tag (car con))
            (key (string-to-char (cdr con)))) ; We need a keycode for the key.
       (cons tag key)))
   (or tags-alist gtd--context-tags-alist)))

(defun gtd--set-tag-variables ()
  "Set all of the tag variables. Run this after the tag file changes."
  (gtd--load-context-tags)
  (setq org-tag-alist (gtd--generate-org-tag-alist)))


;; PRE-INITIALISATION ------------------------------------------------
;; - DIRECTORY CREATION -
(make-directory gtd-active-dir t)
(make-directory gtd-dormant-dir t)
(make-directory gtd-inactive-dir t)

;; - FILE CREATION -
(xtouch-new gtd-context-tags-fpath) ; We want a tag file to exist.

;; - TAG LOADING -
;; Load the tags from file into a global.
(gtd--load-context-tags)

;; Watch the tags file. Update the global upon change.
(when (version<= "24.4" emacs-version)
  (file-notify-add-watch ; Must (require 'filenotify)
   gtd-context-tags-fpath
   '(change)
   (lambda (event)
     (when (eq (cadr event) 'changed)
       (gtd--set-tag-variables)))))

;; Built a tag menu from the global.
(gtd--refresh-tag-menu)
;; Watch the tags global for changes then refresh the menu.
(add-variable-watcher 'gtd--context-tags-alist #'gtd--refresh-tag-watcher)


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

  ;; These rely on roam functions to register IDs.
  (xroam-node-create-at-path gtd-inbox-fpath "inbox for GTD")
  (xroam-node-create-at-path gtd-tasks-fpath "tasks for GTD")
  (xroam-node-create-at-path gtd-someday-or-maybe-fpath "someday or maybe for GTD")
  (xroam-node-create-at-path gtd-trash-fpath "trash for GTD")
  (xroam-node-create-at-path gtd-read-later-fpath "read later for GTD")
  (xroam-node-create-at-path gtd-tickler-fpath "tickler for GTD"))


;; KEYMAPS -----------------------------------------------------------
(global-unset-key (kbd "M-t"))

;; - NORMAL KEYMAPS -
(map! :leader
      (:prefix "j"
       :n "P" #'gtd-project-create
       :n "T" #'gtd-tag-file-edit
       (:prefix "r"
        :desc "Consume Later" :n "c" #'gtd-refile-to-read-later
        :desc "Project" :n "p" #'gtd-refile-to-project
        :desc "Someday or Maybe" :n "s" #'gtd-refile-to-someday-or-maybe
        :desc "Tasks" :n "t" #'gtd-refile-to-tasks
        :desc "Tickler" :n "k" #'gtd-refile-to-tickler
        :desc "Trash" :n "x" #'gtd-refile-to-trash)
       (:prefix "o"
        :desc "Consume Later" :n "c" #'gtd-file-read-later-open
        :desc "Inbox" :n "i" #'gtd-file-inbox-open
        :desc "Project" :n "p" #'gtd-file-project-open
        :desc "Someday or Maybe" :n "s" #'gtd-file-someday-or-maybe-open
        :desc "Tasks" :n "t" #'gtd-file-tasks-open
        :desc "Tickler" :n "k" #'gtd-file-tickler-open
        :desc "Trash" :n "x" #'gtd-file-trash-open)))

;; - ORG AGENDA KEYMAPS -
(after! evil-org-agenda
  (map! :map org-agenda-mode-map
        "M-t" #'gtd-toggle-tags/body))

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

  (setq org-refile-allow-creating-parent-nodes t)

  ;; Refile to the top of the target file.
  (setq org-reverse-note-order t)

  ;; - TAGS -
  ;; These are selectable via (org-set-tags-command) or (counsel-org-tag).
  (setq org-tag-alist (gtd--generate-org-tag-alist))

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
