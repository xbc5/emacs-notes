;; -*- lexical-binding: t; -*-
(require 'neutron-constants)
(require 'org-id)
(require 'org-roam-db)

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

(provide 'neutron-org-roam)
;;; neutron-org-roam.el ends here.
