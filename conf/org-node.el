;; -*- lexical-binding: t; -*-

(after! org-mem
  ;; The indexing backend for org-node.
  ;; Sync with org-id so existing roam IDs are recognised.
  (setq org-mem-do-sync-with-org-id t
        org-mem-watch-dirs (list org-directory))
  (org-mem-updater-mode))

;; Configure org-node with org-roam-compatible defaults, because notes are in
;; roam format, so we preserve slug style, timestamps, and capture behaviour.
(after! org-node
  (setq org-node-creation-fn         #'org-node-new-via-roam-capture
        org-node-file-slug-fn        #'org-node-slugify-like-roam-default
        org-node-file-timestamp-format "%Y%m%d%H%M%S-")
  (org-node-cache-mode)
  ;; Reuse the org-roam backlinks buffer, since notes are already in roam format.
  (org-node-roam-accelerator-mode)
  (map! "M-o n" org-node-global-prefix-map)
  (after! org
    (map! :map org-mode-map
          "M-o n" org-node-org-prefix-map)))
