(defun dnd-unescape-uri () nil) ;; BUG: something something undefined

;; org-ql for some reason triggers this function, but
;; the author says that their package doesn't use it --
;; it seems to be a bug with Emacs:
;; https://github.com/alphapapa/org-ql/issues/345
(defalias 'byte-run--set-speed
  #'(lambda (f _args val)
      (list 'function-put (list 'quote f)
            ''speed (list 'quote val))))

;; (after! org-roam
;;   ;; batch all SQL operations as a single transaction (fixes slow file saves).
;;   (advice-add 'org-roam-db-update-file
;;               :around
;;               (defun +org-roam-db-update-file (fn &rest args)
;;                 (emacsql-with-transaction (org-roam-db)
;;                   (apply fn args)))))

;; When saving, Roam will resolve roam:links. When there's a conflict,
;;  this will prompt with a fuzzy finder to resolve each one.
(defun +org-roam-node-from-title-or-alias (s)
  "Prompt the user to select a node on conflict."
  (let ((matches (seq-uniq
                  (append
                   (org-roam-db-query [:select [id] :from nodes
                                       :where (= title $s1)]
                                      s)
                   (org-roam-db-query [:select [node-id] :from aliases
                                       :where (= alias $s1)]
                                      s)))))
    (cond
     ((seq-empty-p matches)
      nil)
     ((= 1 (length matches))
      (org-roam-populate (org-roam-node-create :id (caar matches))))
     ((> (length matches) 1)
      (org-roam-node-read s nil nil t "Resolve conflict: "))
     (t
      (user-error "Multiple nodes exist with title or alias \"%s\"" s)))))

(advice-add 'org-roam-node-from-title-or-alias
            :override #'+org-roam-node-from-title-or-alias)
