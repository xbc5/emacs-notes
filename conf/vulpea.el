(defun xvulpea--capture-project (node)
  (let* ((title (org-roam-node-title node))
         (meta (xvulpea--project-meta-defaults (ht))))
    (unless xvulpea--project-current (error "Current project not set"))
    (vulpea-create title
                   (xroam-new-fpath title
                                    (f-join xvulpea--project-dir-name
                                            xvulpea--project-current))
                   :properties (xvulpea--make-props meta)
                   :tags (xvulpea--tagify-meta meta (xroam-tag-list '("todo")) 'note-category)
                   :body xvulpea--typical-body)))

(defun xvulpea--capture-concept (node)
  (let* ((title (org-roam-node-title node))
         (meta (xvulpea--concept-meta-defaults (ht))))
    (vulpea-create title
                   (xroam-new-fpath title (ht-get meta 'note-type))
                   :properties (xvulpea--make-props meta)
                   :tags (xvulpea--tagify-meta meta (xroam-tag-list '("todo")) 'note-category)
                   :body xvulpea--typical-body)))
