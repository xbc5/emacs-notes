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
(require 'org-super-agenda)
(require 'f)
(require 'seq)
(require 'neutron-org-roam)

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
         (neutron-files (neutron--get-agenda-files))
         (habits-file (f-join neutron-dir "habits.org"))
         ;; Include habits.org if it exists.
         (extra (when (f-exists-p habits-file) (list habits-file))))
    (setq org-agenda-files (append non-neutron neutron-files extra))))

(defun neutron--setup-todo-keywords ()
  "Configure org todo keywords and faces for neutron.
Appends to existing configuration rather than replacing it."
  (add-to-list 'org-todo-keywords
               '(sequence "NEXT" "TODO" "WAIT" "|" "DONE" "DROP"))
  (dolist (face '(("NEXT" . "magenta")
                  ("TODO" . "cyan1")
                  ("WAIT" . "brown")
                  ("DONE" . "grey")
                  ("DROP" . "grey")))
    ;; Remove any existing entry for this keyword before adding, so reloads don't duplicate.
    (setq org-todo-keyword-faces
          (assoc-delete-all (car face) org-todo-keyword-faces))
    (add-to-list 'org-todo-keyword-faces face)))

(defun neutron--setup-agenda ()
  "Configure neutron `'org-agenda' integration.
Registers agenda commands, enables org-habit and org-super-agenda,
and advises `org-agenda' to refresh agenda files before building."
  (add-to-list 'org-modules 'org-habit)
  (require 'org-habit)
  (advice-add 'org-agenda :before #'neutron--refresh-agenda-files)
  ;; org-super-agenda-mode is a global minor mode; without it,
  ;; :org-super-agenda-groups is ignored.
  (org-super-agenda-mode 1)
  ;; Remove agenda commands before re-adding to prevent duplicates on repeated
  ;; loads.
  (dolist (cmd neutron-agenda-commands)
    (setq org-agenda-custom-commands
          (assoc-delete-all (car cmd) org-agenda-custom-commands)))
  (setq org-agenda-custom-commands
        (append neutron-agenda-commands org-agenda-custom-commands)))

(provide 'neutron-agenda)
;;; neutron-agenda.el ends here.
