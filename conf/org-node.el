;; -*- lexical-binding: t; -*-

;; ----- HELPERS ------------------------------------------------------
;;
(defun my/org-node-affixation-fn (node title)
  "Prepend NODE outline path to TITLE, put tags and ID at frame edge."
  ;; Build the ancestor path (file > heading > foo) as a styled string
  ;; prefixed to the completion candidate.
  (let* ((olp (when (org-mem-subtree-p node)
                (apply #'concat
                       (let (parts)
                         (dolist (anc (org-mem-olpath-with-file-title node) (nreverse parts))
                           (push (propertize anc 'face 'org-node-parent) parts)
                           (push " > " parts))))))
         ;; Style the tags, so they're visually distinct.
         (tags (when-let* ((ts (org-mem-tags node)))
                 (propertize (concat ":" (string-join ts ":") ":") 'face 'org-node-tag)))
         ;; Collapse the ID to zero width, so it's matchable but invisible.
         (id  (propertize (org-mem-id node) 'display ""))
         ;; Push tags to the frame edge by padding with spaces, excluding the
         ;; zero-width ID so it doesn't affect the calculation.
         ;; ?\s is the integer character code for space (32), because `make-string'
         ;; requires an integer.
         (pad (make-string (max 2 (- (frame-width)
                                     (string-width title)
                                     (if olp (string-width olp) 0)
                                     (if tags (string-width tags) 0)
                                     (fringe-columns 'right)
                                     (fringe-columns 'left)))
                           ?\s)))
    ;; Return the affixation triplet: candidate, prefix, and suffix.
    (list title olp (concat pad tags id))))

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

;; ----- CONFIGURATION ------------------------------------------------
;;
;; - KEYMAPS -
(map! "M-n" org-node-global-prefix-map
      :map org-mode-map
      "M-n" org-node-org-prefix-map)

;; - CONFIG -
(after! org-mem
  ;; The indexing backend for org-node.
  ;; Sync with org-id so existing roam IDs are recognised.
  (setq org-mem-do-sync-with-org-id t
        org-mem-watch-dirs (list org-directory))
  (org-mem-updater-mode))


;; Configure org-node with org-roam-compatible defaults, because notes are in
;; roam format, so we preserve slug style, timestamps, and capture behaviour.
(after! org-node
  (setq org-node-creation-fn         #'org-capture
        org-node-file-timestamp-format "%Y%m%d%H%M%S-"
        ;; Change the completion candidates to include more unique data.
        ;; For example, the file ID, tags, and full heading path.
        ;; Without this, completion functions collapse identical results.
        org-node-alter-candidates t
        org-node-affixation-fn #'my/org-node-affixation-fn)
  (org-node-cache-mode)
  ;; Reuse the org-roam backlinks buffer, since notes are already in roam format.
  (org-node-roam-accelerator-mode)
  ;; Prompt for filetags on new nodes created via capture.
  (add-hook 'org-node-creation-hook #'org-node-add-tags)

  (setq org-capture-templates
        '(("n" "Note"
           plain (function org-node-capture-target)
           (function my/org-capture-template-with-placeholder)
           :empty-lines 1
           :jump-to-captured t)

          ("s" "Stub"
           plain (function org-node-capture-target)
           (function my/org-capture-template)
           :empty-lines 1
           :immediate-finish t))))
