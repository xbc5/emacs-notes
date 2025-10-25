;;; conf/mu4e.el -*- lexical-binding: t; -*-

;; Loads untracked private configuration values.
(my/email--load-config-file)

(after! mu4e
        ;; Auto-updates
        (setq mu4e-update-interval 30))
