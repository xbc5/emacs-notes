;; -*- lexical-binding: t; -*-
(require 'neutron-constants)
(require 'neutron-org-roam)
(require 'neutron-hooks)
(require 'neutron-agenda)

(mkdir neutron-dir t)

(defun neutron--setup-auto-index ()
  "Wire `neutron--sync-index-links' to `before-save-hook' for org-roam files."
  (add-hook 'before-save-hook
            (lambda ()
              ;; "neutron--syncing" guards against recursive calls.
              (when (and (not neutron--syncing)
                         (neutron--roam-like-file-p))
                (let ((neutron--syncing t))
                  (neutron--sync-index-links))))))

(with-eval-after-load 'org
  (neutron--setup-agenda)
  (neutron--setup-todo-keywords)
  (setq org-priority-faces '((?A . "red")
                             (?B . "yellow")
                             (?C . "green"))))

(defun neutron--setup-roam-like-backend ()
  "Set up hooks and advice that depend on the active note-taking backend."
  (when neutron-auto-index (neutron--setup-auto-index))
  (advice-add 'delete-file :after #'neutron--on-delete-file))

;; Run immediately if the backend is already loaded, otherwise defer.
(if (featurep neutron-note-platform)
    (neutron--setup-roam-like-backend)
  (with-eval-after-load (symbol-name neutron-note-platform)
    (neutron--setup-roam-like-backend)))


(provide 'neutron-init)
