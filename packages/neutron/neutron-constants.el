;; -*- lexical-binding: t; -*-

;; Configurable.
(defvar neutron-dir "~/org/neutron" "The root directory where all neutron projects live.")
(defvar neutron-auto-index t "Automatically populate indexes with project links.")

;; Global state. Do not modify these.
(defvar neutron--syncing nil "Prevent the index sync hook from recursing on save. t when a sync is in progress.")

(provide 'neutron-constants)
;;; neutron-constants.el ends here.
