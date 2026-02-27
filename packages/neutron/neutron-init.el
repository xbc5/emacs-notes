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
                         (derived-mode-p 'org-mode)
                         (neutron--roam-like-file-p))
                (let ((neutron--syncing t))
                  (neutron--sync-index-links))))))

(with-eval-after-load 'org
  (neutron--setup-agenda)
  (neutron--setup-todo-keywords)
  (setq org-priority-faces '((?A . "red")
                             (?B . "yellow")
                             (?C . "green"))))

;; Run setup after the roam-like backend loads.
(with-eval-after-load neutron-note-platform ; 'org-roam or 'org-node
  (when neutron-auto-index (neutron--setup-auto-index))
  (advice-add 'delete-file :after #'neutron--on-delete-file))


(provide 'neutron-init)
