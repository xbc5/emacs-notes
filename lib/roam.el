(defun my/roam-tag-list ()
  (let ((crm-separator "[ 	]*:[ 	]*"))
    (completing-read-multiple "Roam tags: " (org-roam-tag-completions))))

(defun my/roam-set-brief ()
  (interactive)
  (org-set-property "BRIEF"
                    (read-string "Set brief: "
                                 (my/roam-property-values "BRIEF"))))

(defun my/roam-property-values (key)
  (cdr
   (assoc key (org-roam-node-properties
               (org-roam-node-at-point)))))
