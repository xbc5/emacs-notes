;; -*- lexical-binding: t; -*-
(require 'neutron-constants)
(require 'neutron-init)
(require 'neutron-ui)
(require 'neutron-org-roam)

;; Create the root project directory
(defun neutron--create-root-dir ()
  "Create the neutron root directory with parent directories as needed."
  (mkdir neutron-dir t))

(defun neutron--get-dirs (root)
  "Return all non-hidden directories recursively under ROOT."
  (seq-filter #'file-directory-p
              (directory-files-recursively root "^[^.]" t)))

(defun neutron--pick-project-dir (message &optional require-match include-root)
  "Let user pick a project directory from available subdirectories.

MESSAGE is shown in the prompt.
REQUIRE-MATCH forces selection from the list.
INCLUDE-ROOT adds the root directory as an option."
  (let* ((dirs (neutron--get-dirs neutron-dir))
         ;; Strip prefix relative to `neutron-dir'.
         (relative (mapcar (lambda (d) (file-relative-name d neutron-dir)) dirs))
         ;; Add <root> as a selection if `include-root' is set.
         (choices (if include-root (cons "<root>" relative) relative))
         (choice (completing-read message choices nil require-match)))
    ;; Return full file path.
    (if (equal choice "<root>")
        neutron-dir
      (f-join neutron-dir choice))))

(defun neutron--format-path (path)
  "Return PATH as a relative path from neutron-dir for display."
  (file-relative-name path neutron-dir))

(defun neutron--slugify (str)
  "Lowercase, trim, and replace whitespace with dashes."
  (replace-regexp-in-string "\\s-+" "-" (downcase (string-trim str))))

(defun neutron--touch (path)
  "Create an empty file at PATH."
  (write-region "" nil path t))

(defun neutron--move-dir (from to)
  "Move a directory from FROM to TO, updating buffers and org-roam."
  ;; Collect org files before renaming, because FROM won't exist after.
  (let ((old-files (directory-files-recursively from "\\.org$")))
    (rename-file from to)
    (dolist (old-file old-files)
      (let ((new-file (f-join to (file-relative-name old-file from))))
        ;; Update visiting buffers to point to the new path.
        (when-let ((buf (find-buffer-visiting old-file)))
          (with-current-buffer buf
            (set-visited-file-name new-file t t)))
        ;; Sync org-roam DB.
        (when (featurep 'org-roam)
          (ignore-errors (org-roam-db-autosync--rename-file-a old-file new-file)))))))

(defun neutron-create-project ()
  "Create a new neutron project."
  (interactive)
  (let* ((parent (neutron--pick-project-dir "DESTINATION directory: " t t))
         (project-title (neutron--prompt "Project TITLE: "))
         (dir-name (neutron--slugify project-title))
         (full-path (f-join parent dir-name))
         (index-path (f-join full-path "index.org")))
    (mkdir full-path t)
    (neutron--create-roam-node index-path project-title)))

(defun neutron-delete-project ()
  "Delete a neutron project."
  (interactive)
  (let* ((project (neutron--pick-project-dir "DELETE project: " t))
         (confirm (y-or-n-p (format "Delete %s? " (neutron--format-path project)))))
    (when confirm
      (dolist (file (directory-files-recursively project "\\.org$"))
        (when-let ((buf (find-buffer-visiting file)))
          (kill-buffer buf)))
      (delete-directory project t))))

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
          (neutron--move-dir project new-path))))))

(defun neutron--is-index (&optional file-path)
  "Return non-nil if FILE-PATH is an index file (index.org) within neutron-dir.
FILE-PATH is optional and defaults to the buffer file name."
  (let ((file (or file-path (buffer-file-name))))
    (and (file-in-directory-p file neutron-dir)
         (string= (f-filename file) "index.org"))))

(defun neutron--is-sibling (&optional file-path)
  "Return non-nil if FILE-PATH is a sibling (non-index file) within neutron-dir.
FILE-PATH is optional and defaults to the buffer file name."
  (let ((file (or file-path (buffer-file-name))))
    (and (file-in-directory-p file neutron-dir)
         (not (string= (f-filename file) "index.org")))))

(defun neutron--file-type (&optional file-path)
  "Return 'index or 'sibling for FILE-PATH within neutron-dir.
FILE-PATH defaults to the buffer file name."
  (if (neutron--is-index file-path)
      'index
    'sibling))

(defun neutron--get-parent-index (&optional file-path)
  "Return the path to the parent index.
FILE-PATH is where to start looking (defaults to buffer file name).
Returns nil if the file is not found or is outside neutron-dir."
  (let* ((parent-dir
          (f-parent (f-dirname (expand-file-name
                                (or file-path (buffer-file-name))))))
         (parent-index (f-join parent-dir "index.org")))
    ;; Only return if parent index exists and is within neutron-dir.
    (when (and (f-exists-p parent-index)
               (f-ancestor-of-p neutron-dir parent-index))
      parent-index)))

(defun neutron--get-child-indexes (&optional file-path)
  "Return a list of absolute paths to immediate child indexes.
FILE-PATH is where to start looking (defaults to buffer file name).
Returns nil if the file is not found or is outside neutron-dir."
  (let* ((buffer-dir
          (f-dirname
           (or file-path (buffer-file-name)))))
    (when (file-in-directory-p buffer-dir neutron-dir)
      (mapcar #'expand-file-name
              (f-glob "*/index.org" buffer-dir)))))

(provide 'neutron-fs)
