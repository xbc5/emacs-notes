;; -*- lexical-binding: t; -*-
(require 'neutron-constants)

(defun neutron--create-roam-node (file-path title)
  "Create an org-roam node for FILE-PATH with TITLE.

FILE-PATH is the absolute path to the org file.
TITLE is the display title for the node."
  (let ((id (org-id-new)))
    (with-temp-file file-path
      (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n#+title: %s\n\n* index\n\n* tasks\n\n* progress\n\n* thoughts\n\n* ideas\n" id title)))
    (org-roam-db-update-file file-path)))

(provide 'neutron-org-roam)
