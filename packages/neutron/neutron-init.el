;; -*- lexical-binding: t; -*-
(require 'neutron-constants)
(require 'neutron-org-roam)
(require 'neutron-hooks)

(defun neutron--setup-auto-index ()
  "Wire `neutron--sync-index-links' to `before-save-hook' for org-roam files."
  (add-hook 'before-save-hook
            (lambda ()
              ;; "neutron--syncing" guards against recursive calls.
              (when (and (not neutron--syncing)
                         (derived-mode-p 'org-mode)
                         (org-roam-file-p))
                (let ((neutron--syncing t))
                  (neutron--sync-index-links))))))

(when neutron-auto-index (neutron--setup-auto-index))
(neutron--setup-agenda)

(advice-add 'delete-file :after #'neutron--on-delete-file)

(provide 'neutron-init)
