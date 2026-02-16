;; -*- lexical-binding: t; -*-
(require 'neutron-constants)
(require 'org-id)
(require 'org-roam-db)

(defun neutron--create-roam-node (file-path title)
  "Create an org-roam node for FILE-PATH with TITLE.

FILE-PATH is the absolute path to the org file.
TITLE is the display title for the node."
  (let ((id (org-id-new))
        (default-status (or (car neutron-project-statuses) "inactive")))
    (with-temp-file file-path
      (insert (concat ":PROPERTIES:\n"
                      ":ID: " id "\n"
                      ":NEUTRON_PROJECT_STATUS: " default-status "\n"
                      ":END:\n"
                      "#+title: " title "\n\n"
                      "* summary\n\n"
                      "* index\n\n"
                      "* tasks\n\n"
                      "* progress\n\n"
                      "* spec\n\n"
                      "* data\n\n"
                      "* thoughts\n\n"
                      "* ideas\n")))
    (org-roam-db-update-file file-path)))

(provide 'neutron-org-roam)
;;; neutron-org-roam.el ends here.
