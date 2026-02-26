;; -*- lexical-binding: t; -*-
(require 'neutron-constants)
(require 'neutron-fs)
(require 'neutron-org)
(require 'neutron-org-roam)
(require 'neutron-ui)
(require 'neutron-agenda)
(require 'f)
(require 'org-roam)
(require 'neutron-init)

(defun neutron-create-project ()
  "Create a new neutron project."
  (interactive)
  (let* ((parent (neutron--pick-project-dir "DESTINATION directory: " t t))
         (project-title (neutron--prompt "Project TITLE: "))
         (dir-name (neutron--slugify project-title))
         (full-path (f-join parent dir-name))
         (index-path (f-join full-path "index.org")))
    (mkdir full-path t)
    (neutron--create-roam-node index-path project-title)
    ;; Save the new index and parent so the synced links are persisted.
    (neutron--save-related-files '(local-index parent-index) index-path)))

(defun neutron-delete-project ()
  "Delete a neutron project."
  (interactive)
  (let* ((project (neutron--pick-project-dir "DELETE project: " t))
         (index-path (f-join project "index.org"))
         (confirm (y-or-n-p (format "Delete %s? " (neutron--format-path project)))))
    (when confirm
      ;; Disconnect removes bidirectional index links from the node and its
      ;; parent/local-index, so the index doesn't have dangling links.
      (neutron--disconnect-node project)
      ;; Force kill the index buffer.
      (when-let ((buf (find-buffer-visiting index-path)))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      ;; Delete the directory after disconnecting.
      (delete-directory project t)
      ;; Save the parent so disconnect's changes are persisted.
      (neutron--save-related-files '(parent-index) index-path))))

(defun neutron-move-project ()
  "Move a neutron project to another directory."
  (interactive)
  (let* ((project (neutron--pick-project-dir "SOURCE project: " t))
         (destination (neutron--pick-project-dir "DESTINATION directory: " t t))
         (project-name (file-name-nondirectory project))
         (new-path (f-join destination project-name)))
    (if (file-exists-p new-path)
        (message "Collision: \"%s\" already exists in \"%s\"." project-name (neutron--format-path destination))
      (let ((confirm (y-or-n-p (format "Move %s to %s? "
                                       (neutron--format-path project)
                                       (neutron--format-path new-path)))))
        (when confirm
          (let ((old-index (f-join project "index.org"))
                (new-index (f-join new-path "index.org")))
            ;; Disconnect removes bidirectional index links from the node and its
            ;; parent/local-index, so it can be re-synced fresh at the new location.
            (neutron--disconnect-node project)
            ;; Save the old parent so disconnect's changes are persisted.
            (neutron--save-related-files '(parent-index) old-index)
            (neutron--move-dir project new-path)
            ;; Save the new index and parent so the re-synced links are persisted.
            (neutron--save-related-files '(local-index parent-index) new-index)))))))

(defun neutron-rename-project ()
  "Rename the current buffer's project directory and title.
Updates the directory name (slug-based), the #+title: keyword,
and refreshes all bidirectional index links.
Signals an error if a project with the new name already exists."
  (interactive)
  (let* ((index-path (or (neutron--get-project-index)
                         (user-error "Not in a Neutron project")))
         (new-title (neutron--prompt "New project name: "))
         (new-slug (neutron--slugify new-title))
         (project-dir (f-parent index-path))
         (new-dir (f-join (f-parent project-dir) new-slug))
         (new-index (f-join new-dir "index.org")))
    (if (f-exists-p new-dir)
        (message "Project '%s' already exists." new-slug)
      ;; Rename the directory and update visiting buffers.
      (neutron--move-dir project-dir new-dir)
      ;; Update the #+title: in the index file.
      (neutron--set-title new-title new-index)
      ;; Refresh all bidirectional links so the new title propagates.
      (neutron--sync-index-links new-index)
      ;; Save the new index and all related files.
      (neutron--save-related-files '(all) new-index))))

(defun neutron-create-sibling ()
  "Find or create an org-roam node, scoped to the current project directory.
Saving the file triggers an index sync."
  (interactive)
  (let* ((current-dir (f-dirname (buffer-file-name)))
         (templates `(("s" "sibling" plain
                       ;; Cursor starts in summary.
                       ,(string-replace "* summary\n" "* summary\n%?\n" neutron--node-headings)
                       :target (file+head ,(concat current-dir "/${slug}.org")
                                          ,(concat (neutron--properties-drawer)
                                                   "#+title: ${title}\n"))
                       :empty-lines 1 ;; Newline before/after the capture body (headings).
                       :unnarrowed t))))
    (org-roam-node-find nil nil
                        (lambda (comp-candidate)
                          (when-let ((file (org-roam-node-file (cdr comp-candidate))))
                            (f-same-p (f-dirname file) current-dir)))
                        nil :templates templates)))

(defun neutron-create-task (&optional current-project)
  "Create a new task in a neutron project's index.org.
CURRENT-PROJECT, if non-nil, skips the finder and uses the current project.
Opens a capture buffer with TODO [#C] format."
  (interactive)
  ;; Use current project directly, or show the finder.
  (let ((target-file (if current-project
                         (or (ignore-errors (neutron--get-project-index))
                             neutron--last-selected-project-index)
                       (neutron--find-project))))
    (when target-file
      ;; Ensure the tasks heading exists before capturing.
      (save-excursion
        (neutron--ensure-heading target-file "tasks"))
      ;; Dynamically bind a temporary capture template.
      (let ((org-capture-templates
             `(("t" "Task" entry
                (file+headline ,target-file "tasks")
                "** TODO [#C] %?"
                :prepend nil))))
        (org-capture nil "t")))))

(defun neutron-create-habit ()
  "Create a new habit in habits.org at the root of `neutron-dir'."
  (interactive)
  (let ((target-file (f-join neutron-dir "habits.org")))
    (unless (f-exists-p target-file)
      (neutron--create-roam-node target-file "habits"))
    (neutron--ensure-heading target-file "habits")
    (let ((schedule (neutron--pick-schedule)))
      (letrec ((hook (lambda ()
                       (remove-hook 'org-capture-after-finalize-hook hook)
                       (when org-capture-last-stored-marker
                         (with-current-buffer (marker-buffer org-capture-last-stored-marker)
                           (save-excursion
                             (goto-char org-capture-last-stored-marker)
                             (neutron--set-org-properties '(("STYLE" . "habit")))
                             (org-schedule nil schedule)))))))
        (add-hook 'org-capture-after-finalize-hook hook))
      (let ((org-capture-templates
             `(("h" "Habit" entry
                (file+headline ,target-file "habits")
                "** TODO %?"
                :prepend nil))))
        (org-capture nil "h")))))

(defun neutron-refile-tasks ()
  "Refile selected headings to a target under * tasks in a neutron index.
Uses the active region if set; otherwise uses the heading at point."
  (interactive)
  (neutron--validate-refile-region)
  (let* ((targets (neutron--get-task-targets))
         (choice (completing-read "Refile to: " (mapcar #'car targets) nil t))
         (rfloc (seq-find (lambda (target) (string= (car target) choice)) targets)))
    ;; org-refile handles the active region natively.
    (org-refile nil nil rfloc)
    ;; Save all modified neutron buffers once at the end.
    (neutron--save-modified-buffers)))

(provide 'neutron)
;;; neutron.el ends here.
