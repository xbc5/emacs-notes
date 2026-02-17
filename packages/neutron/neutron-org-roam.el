;; -*- lexical-binding: t; -*-
(require 'neutron-constants)
(require 'org-id)
(require 'org-roam-db)
(require 'f)

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
  (with-temp-file file-path
    (insert (concat (neutron--properties-drawer)
                    "#+title: " title "\n\n"
                    neutron--node-headings))
    (org-roam-db-update-file file-path)))

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

(provide 'neutron-org-roam)
;;; neutron-org-roam.el ends here.
