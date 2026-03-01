;; -*- lexical-binding: t; -*-
(require 'neutron-constants)
(require 'neutron-fs)
(require 'neutron-org)
(require 'org-id)

(defun neutron--org-mem-file-p (&optional file)
  "Return t if FILE is indexed by org-mem, nil otherwise.
FILE defaults to the current buffer's file path."
  (when-let ((path (or file (buffer-file-name (buffer-base-buffer)))))
    (not (null (gethash (file-truename path) org-mem--truename<>metadata)))))

(defun neutron--roam-like-file-p (&optional file)
  "Return t if FILE is tracked by the active note-taking backend.
FILE defaults to the current buffer's file path."
  (cond
   ((eq neutron-note-platform 'org-node)
    (require 'org-mem)
    (neutron--org-mem-file-p file))
   ((eq neutron-note-platform 'org-roam)
    (require 'org-roam)
    (org-roam-file-p file))))

(defun neutron--roam-like-node-find (current-dir templates)
  "Find or create a node scoped to CURRENT-DIR.
CURRENT-DIR is the directory to filter and create nodes in.
TEMPLATES is the org-roam capture templates list."
  (cond
   ((eq neutron-note-platform 'org-node)
    (require 'org-node)
    (require 'org-mem)
    (let ((org-node-filter-fn
           (lambda (node)
             (f-same-p (f-dirname (org-mem-entry-file node)) current-dir)))
          (org-node-creation-fn
           (lambda ()
             (let ((file-path (f-join current-dir
                                      (concat (format-time-string org-node-file-timestamp-format)
                                              (neutron--slugify org-node-proposed-title)
                                              ".org"))))
               (neutron--create-roam-node file-path org-node-proposed-title)))))

      (org-node-find)))
   ((eq neutron-note-platform 'org-roam)
    (require 'org-roam)
    (org-roam-node-find nil nil
                        (lambda (comp-candidate)
                          (when-let ((file (org-roam-node-file (cdr comp-candidate))))
                            (f-same-p (f-dirname file) current-dir)))
                        nil :templates templates))))

(defun neutron--roam-like-db-rename-file (old-file new-file)
  "Update the node database after renaming OLD-FILE to NEW-FILE.
OLD-FILE is the previous absolute path.
NEW-FILE is the new absolute path.
For org-node, this is a no-op because `org-mem-updater-mode' handles it.
For org-roam, syncs its SQLite DB."
  (cond
   ((eq neutron-note-platform 'org-node) nil)
   ((eq neutron-note-platform 'org-roam)
    (require 'org-roam-db)
    (org-roam-db-autosync--rename-file-a old-file new-file))))

(defun neutron--roam-like-db-update-file (file-path)
  "Update the node database for FILE-PATH.
FILE-PATH is the absolute path to the org file to index.
For org-node, triggers `org-mem-updater-update' so the new file is indexed
before the save hook runs. For org-roam, syncs its SQLite DB."
  (cond
   ((eq neutron-note-platform 'org-node)
    (require 'org-mem-updater)
    (let ((inhibit-message t))
      (org-mem-updater-update t)))
   ((eq neutron-note-platform 'org-roam)
    (require 'org-roam-db)
    (org-roam-db-update-file file-path))))

(defun neutron--properties-drawer (&optional status id)
  "Render a properties drawer.
STATUS is the status value (e.g., \"inactive\")."
  (concat ":PROPERTIES:\n"
          ":ID: " (or id (org-id-new)) "\n"
          ":NEUTRON_PROJECT_STATUS: " (or status (car neutron-project-statuses) "inactive") "\n"
          ":END:\n"))

(defun neutron--create-roam-node (file-path title)
  "Create an org-roam node for FILE-PATH with TITLE.

FILE-PATH is the absolute path to the org file.
TITLE is the display title for the node."
  ;; Create a physical file first, then we'll index it.
  (with-temp-file file-path
    (insert (concat (neutron--properties-drawer)
                    "#+title: " title "\n\n"
                    neutron--node-headings)))
  ;; Update the db after the file is written.
  (neutron--roam-like-db-update-file file-path))

(defun neutron--delete-relevant-index-links (&optional file)
  "Remove all index links relevant to FILE.
Returns the file type (`'index or `'sibling), or nil if not a Neutron file."
  (when (neutron--is-neutron-file-p file)
    (when-let ((id (neutron--get-id-from-file-path file)))
      (let ((file-type (neutron--file-type file)))
        (pcase file-type
          ('sibling
           ;; Remove from local index siblings list.
           (when-let ((local-index (neutron--get-local-index file)))
             (neutron--remove-index-link local-index id)))
          ('index
           ;; Remove from parent's children list.
           (when-let ((parent-index (neutron--get-parent-index file)))
             (neutron--remove-index-link parent-index id))
           ;; Remove from each sibling's home link.
           (dolist (sibling (neutron--get-siblings file))
             (neutron--remove-index-link sibling id))))
        file-type))))

(defun neutron--query-index-nodes ()
  "Return (FILE PROPERTIES) pairs for index.org files under `neutron-dir'."
  (let ((dir (file-name-as-directory (expand-file-name neutron-dir))))
    (cond
     ((eq neutron-note-platform 'org-node)
      (require 'org-mem)
      (seq-filter
       (lambda (row) (string= (f-filename (car row)) "index.org"))
       (mapcar (lambda (entry)
                 (list (org-mem-entry-file entry)
                       (org-mem-entry-properties entry)))
               (seq-filter
                (lambda (entry)
                  (and (= (org-mem-entry-level entry) 0)
                       (string-prefix-p dir (org-mem-entry-file entry))))
                (org-mem-all-id-nodes)))))
     ((eq neutron-note-platform 'org-roam)
      (require 'org-roam-db)
      (seq-filter
       (lambda (row) (string= (f-filename (car row)) "index.org"))
       (org-roam-db-query
        [:select [file properties]
         :from nodes
         :where (and (= level 0)
                     (like file $s1))]
        (concat dir "%")))))))

(provide 'neutron-org-roam)
;;; neutron-org-roam.el ends here.
