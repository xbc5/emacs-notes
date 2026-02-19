;; -*- lexical-binding: t; -*-
(require 'neutron-fs)
(require 'neutron-org-roam)

(defun neutron--on-delete-file (file &optional _trash)
  "Remove index links after FILE is deleted and save affected indexes.
FILE is the file path that was deleted.
_TRASH is ignored — cleanup runs whether the file is trashed or deleted."
  (pcase (neutron--delete-relevant-index-links file)
    ('sibling (neutron--save-related-files '(local-index) file))
    ('index (neutron--save-related-files '(parent-index siblings) file))))

(provide 'neutron-hooks)
