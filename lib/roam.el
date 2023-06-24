(defun my/roam-tag-list ()
  (let ((crm-separator "[ 	]*:[ 	]*"))
    (completing-read-multiple "Roam tags: " (org-roam-tag-completions))))

(defun my/roam-set-brief ()
  (interactive)
  (org-set-property "BRIEF"
                    (read-string "Set brief: "
                                 (org-property-values "BRIEF"))))
