;; -*- lexical-binding: t; -*-

;; Configurable.
(defvar neutron-dir "~/org/neutron" "The root directory where all neutron projects live.")
(defvar neutron-auto-index t "Automatically populate indexes with project links.")
(defvar neutron-project-statuses '("inactive" "incubating" "active" "tickler") "List of valid project statuses. The first is the default.")

;; Global state. Do not modify these.
(defvar neutron--syncing nil "Prevent the index sync hook from recursing on save. t when a sync is in progress.")
(defvar neutron--last-selected-project-index nil "Last selected project index file path, so the finder can pre-select it.")

(provide 'neutron-constants)
;;; neutron-constants.el ends here.
