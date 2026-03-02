;; -*- lexical-binding: t; -*-
(require 'f)

;; ----- HELPERS ------------------------------------------------------
;;
(defun my/org-node-completion-tags (node)
  "Return a styled tag string for NODE, including an implicit @section tag.
Derives @section from the first path segment of NODE's file relative to
`org-directory', then merges with real tags into a single `:tag1:tag2:' string.
NODE: an org-mem node."
  (let* ((path-tag (concat "@" (car (f-split (f-relative (org-mem-entry-file node) org-directory)))))
         (all (cons path-tag (org-mem-tags node))))
    (propertize (concat ":" (string-join all ":") ":") 'face 'org-node-tag)))

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

         (tags (my/org-node-completion-tags node))
         ;; Collapse the ID to zero-width, so it's matchable but invisible.
         (id  (propertize (org-mem-id node) 'display ""))
         ;; Push tags and ID to the frame edge by padding with spaces, excluding
         ;; the zero-width ID so it doesn't affect the calculation.
         ;; ?\s is the integer character code for space (32), because `make-string'
         ;; requires an integer.
         (pad (make-string (max 2 (- (frame-width)
                                     (string-width title)
                                     (if olp (string-width olp) 0)
                                     (string-width tags)
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

(defun my/org-node-context-follow-mode ()
  "Toggle all follow modes."
  (interactive)
  (if (or org-node-context-follow-mode org-node-context-follow-mode)
      ;; If any are enabled, disable them all.
      (progn
        (org-node-context-follow-mode -1) ; Context follows between buffers.
        (org-node-context-follow-local-mode -1)) ; Context follows between buffer-level nodes.
    (progn
      ;; If they're all disabled, then enable them all.
      (org-node-context-follow-mode 1)
      (org-node-context-follow-local-mode 1))))

(defun my/org-node-disable-context-buffer ()
  "Force disable the context buffer without."
  (interactive)
  ;; I got this code from `'org-node-context-toggle'.
  (if-let* ((win (get-buffer-window org-node-context-main-buffer 'visible)))
      (quit-window nil win)))

;; ----- CONFIGURATION ------------------------------------------------
;;
(use-package! org-mem
  :defer (not (string= (getenv "EMACS_MODE") "notes")) ; Defer only when not explicitly notes.
  :init
  (unless (bound-and-true-p org-directory)
    (setq org-directory "~/org"))

  ;; A special file that caches all org-ids, used by the indexer.
  (setq org-id-locations-file (f-join org-directory ".orgids")
        org-mem-do-sync-with-org-id t
        org-mem-watch-dirs (list org-directory))
  :config
  (require 'org-id)
  (org-id-locations-load) ; Load org IDs from file, so the indexer finds them.
  (org-mem-updater-mode) ;; Enable with `org-node-cache-mode'
  (org-node-cache-mode)) ;; Enable with `org-mem-updater-mode'

;; Configure org-node with org-roam-compatible defaults, because notes are in
;; roam format, so we preserve slug style, timestamps, and capture behaviour.
(use-package! org-node
  :init
  (setq neutron-note-platform 'org-node)
  (map! "M-n" #'org-node-find
        :leader
        "n" #'org-node-global-prefix-map ; Org commands for any buffer.
        :map org-mode-map
        "n"   org-node-org-prefix-map)   ; Org commands for Org buffers (extends previous).

  :config
  (map!
   ;; Since the backlinks buffer is not Org mode, map this outside of
   ;; `org-mode-map'. That allows us to disable it while it's selected.
   "M-B" #'my/org-node-disable-context-buffer
   "M-b" #'org-node-context-dwim ; Same with this (backlinks buffer).

   :map org-mode-map
   "M-f" #'my/org-node-context-follow-mode    ; Make backlinks buffer follow note at cursor position.
   "M-I" #'org-node-insert-link)

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
