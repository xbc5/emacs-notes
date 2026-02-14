;; -*- lexical-binding: t; -*-
;;
;;; neutron.el --- A project hierarchy for Org Agenda and Org-roam.
;;;
;;; Commentary:
;;;
;;; Projects are a hierarchical space (directories and files) that contain
;;; (real-life) project-related notes, links, and knowledge. They serve as a
;;; working memory. The goal of Neutron is to auto-manage the hierarchy,
;;; allowing you to create and incubate ideas over time before undertaking work.
;;; Use projects as a central hub to keep you contextually informed.
;;;
;;;
;;; PROJECT ELEMENTS
;;;
;;; Each project has an index.org file, which is the entry point and main
;;; workspace. It contains tasks, notes, data, tables, etc.
;;;
;;; At the top of each index file there's an index section—which Neutron
;;; auto-generates—that links the hierarchy in a graph. The index maintains
;;; links to the parent, children, and siblings, and each link has a summary,
;;; which it gets from the relevant "summary" section of each note.
;;;
;;; Sibling notes are mere workspaces for data, ideas, specifications, and
;;; everything else. They're Org-roam files with no special significance. When
;;; you save one of these files, it creates a two-way index link with the local
;;; index.org file (its sibling index.org).
;;;
;;; Index.org files are special. They're the main hub for a project (or
;;; sub-project). They contain an Org Agenda to-do list.
;;;
;;;
;;; USING PROJECTS
;;;
;;; Neutron's project system is hierarchical, with arbitrarily nested
;;; projects. Start by creating one parent project (e.g., a software app), then
;;; create sub-projects for focused work (like a Git branch). If you find a
;;; sub-project requires significant work, you can further create nested
;;; sub-projects to divide and conquer.
;;;
;;; Project contents are temporary. While the data may last for years, it's not
;;; permanent. Use notes outside of projects for facts that don't belong to them.
;;; The general idea is that projects function as a working memory.
;;;
;;; One of the primary features of Neutron is project management, and so you can
;;; easily create, move, and delete projects. There are three functions to
;;; assist you:
;;;
;;; - 'neutron-create-project'
;;; - 'neutron-move-project'
;;; - 'neutron-delete-project'
;;;
;;; These functions automatically manage the graph links and allow you to freely
;;; restructure projects as you wish.
;;;
(require 'neutron-constants)
(require 'neutron-fs)
(require 'neutron-org)
(require 'neutron-org-roam)
(require 'neutron-ui)

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

(mkdir neutron-dir t)

(defun neutron--setup-auto-index ()
  "Wire `neutron--sync-index-links' to `before-save-hook' for org-roam files."
  (add-hook 'before-save-hook
            (lambda ()
              ;; "neutron--syncing" guards against recursive calls.
              (when (and (not neutron--syncing)
                         (derived-mode-p 'org-mode)
                         (org-roam-file-p))
                (let ((neutron--syncing t))
                  (neutron--sync-index-links))))))

(when neutron-auto-index (neutron--setup-auto-index))


(provide 'neutron)
;;; neutron.el ends here.
