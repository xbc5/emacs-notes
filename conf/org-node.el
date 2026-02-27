;; -*- lexical-binding: t; -*-

(map! "M-n" org-node-global-prefix-map
      :map org-mode-map
      "M-n" org-node-org-prefix-map)

(after! org-mem
  ;; The indexing backend for org-node.
  ;; Sync with org-id so existing roam IDs are recognised.
  (setq org-mem-do-sync-with-org-id t
        org-mem-watch-dirs (list org-directory))
  (org-mem-updater-mode))

(defun my/org-capture-template(&optional cursor-placeholder)
  "Produce an Org-compatible capture template.
CURSOR-PLACEHOLDER: Include a %? under the details heading."
  (let ((template (list "* summary"
                        "* index"
                        "* conclusion"
                        "* details"
                        "* ideas"
                        "* thoughts")))
    (when cursor-placeholder
      (push "%?" (nthcdr 4 template)))
    (s-join "\n" template)))

(defun my/org-capture-template-with-placeholder ()
  (my/org-capture-template t))

;; Configure org-node with org-roam-compatible defaults, because notes are in
;; roam format, so we preserve slug style, timestamps, and capture behaviour.
(after! org-node
  (setq org-node-creation-fn         #'org-capture
        org-node-file-timestamp-format "%Y%m%d%H%M%S-")
  (org-node-cache-mode)
  ;; Reuse the org-roam backlinks buffer, since notes are already in roam format.
  (org-node-roam-accelerator-mode)
  (setq org-capture-templates
        '(("n" "Note"
           entry (function org-node-capture-target)
           (function my/org-capture-template-with-placeholder)
           :empty-lines 1)

          ("q" "Quick"
           plain (function org-node-capture-target)
           (function my/org-capture-template)
           :empty-lines 1
           :immediate-finish t))))
