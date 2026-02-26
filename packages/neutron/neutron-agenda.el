;; -*- lexical-binding: t; -*-
;;
;;; neutron-agenda.el --- Dynamic org-agenda file list from org-roam DB.
;;;
;;; Commentary:
;;;
;;; Org Agenda scans every file in `org-agenda-files', which is slow with
;;; thousands of files. Neutron keeps TODOs in index.org files, each with a
;;; NEUTRON_PROJECT_STATUS property. This module queries the org-roam DB to
;;; build `org-agenda-files' dynamically, including only active and tickler
;;; projects.
;;;
(require 'neutron-constants)
(require 'org-roam-db)
(require 'org-super-agenda)
(require 'f)
(require 'seq)

(defun neutron--query-index-nodes ()
  "Return (FILE PROPERTIES) pairs for index.org files under `neutron-dir'."
  ;; We need to use this function to determine `'NEUTRON_PROJECT_STATUS'.
  ;; Org-roam stores absolute paths and :PROPERTIES: entries in the nodes table.
  ;; Use a prefix-only "like" (e.g., "/home/user/org/neutron/%") so SQLite can
  ;; use an index on the file column, avoiding a full table scan.
  (let* ((dir (file-name-as-directory (expand-file-name neutron-dir)))
         (rows (org-roam-db-query
                [:select [file properties]
                 :from nodes
                 :where (and (= level 0) ;; Level 0 == root nodes (not heading-based nodes)
                             (like file $s1))]
                (concat dir "%"))))
    ;; Filtering for index.org in SQL would require a wildcard-prefixed "like"
    ;; (e.g., "%/index.org"), which prevents SQLite from using an index. We
    ;; therefore cannot filter index.org files in the SQL command, so we do it
    ;; here instead.
    (seq-filter
     (lambda (row) (string= (f-filename (car row)) "index.org"))
     rows)))

(defun neutron--get-agenda-files ()
  "Return index.org paths with active or tickler project status."
  ;; Org-roam stores properties as a serialized alist in the DB, so we can't
  ;; reliably match on NEUTRON_PROJECT_STATUS in SQL. Even with thousands of
  ;; index nodes, an elisp filter processes them in milliseconds, so filtering
  ;; here is preferable to a complex SQL query.
  (let ((rows (neutron--query-index-nodes)))
    ;; Filter for "active" or "tickler" status, then extract file paths.
    (mapcar #'car
            ;; Select only rows with active or tickler status.
            (seq-filter
             (lambda (row)
               (member (cdr (assoc "NEUTRON_PROJECT_STATUS" (cadr row)))
                       '("active" "tickler")))
             rows))))

(defun neutron--refresh-agenda-files (&rest _)
  "Refresh `org-agenda-files' with active neutron index files.
Existing non-neutron entries are preserved."
  ;; Strip old neutron entries from org-agenda-files to avoid duplicates when
  ;; appending fresh ones.
  (let* ((prefix (file-name-as-directory (expand-file-name neutron-dir)))
         (non-neutron (seq-remove
                       ;; Those prefixed by 'neutron-dir'.
                       (lambda (f) (string-prefix-p prefix (expand-file-name f)))
                       org-agenda-files))
         ;; Set new ones.
         (neutron-files (neutron--get-agenda-files)))
    (setq org-agenda-files (append non-neutron neutron-files))))

(defun neutron--register-agenda-commands ()
  "Remove existing neutron keys from `org-agenda-custom-commands' and re-register them.
Prevents duplicate entries on repeated init file loads."
  (dolist (cmd neutron-agenda-commands)
    (setq org-agenda-custom-commands
          (assoc-delete-all (car cmd) org-agenda-custom-commands)))
  (setq org-agenda-custom-commands
        (append neutron-agenda-commands org-agenda-custom-commands)))

(defun neutron--setup-agenda ()
  "Advise `org-agenda' to refresh neutron agenda files before building."
  (advice-add 'org-agenda :before #'neutron--refresh-agenda-files)
  (org-super-agenda-mode 1))

(provide 'neutron-agenda)
;;; neutron-agenda.el ends here.
