;; -*- lexical-binding: t; -*-

;; Configurable.
(defvar neutron-dir "~/org/neutron" "The root directory where all neutron projects live.")
(defvar neutron-auto-index t "Automatically populate indexes with project links.")

;; Global state. Do no modify these.
(defvar neutron--syncing nil "Prevent index sync hook from recursively calling itself upon saves. The value is t when a sync is in progress.")

(provide 'neutron-constants)
