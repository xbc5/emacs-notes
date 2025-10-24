;;; conf/mu4e.el -*- lexical-binding: t; -*-

(my/email--load-config-file)

(after! mu4e
  ;; Auto-updates
  (setq mu4e-update-interval 60))
