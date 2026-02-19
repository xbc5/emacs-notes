;; -*- lexical-binding: t; -*-
(require 'neutron-constants)
(require 'neutron-fs)
(require 'neutron-org)
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

(provide 'neutron-org-roam)
;;; neutron-org-roam.el ends here.
