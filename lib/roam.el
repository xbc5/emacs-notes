(defun my/roam-tag-list ()
  (let ((crm-separator "[ 	]*:[ 	]*"))
    (completing-read-multiple "Roam tags: " (org-roam-tag-completions))))

(defun my/roam-set-brief ()
  (interactive)
  (org-set-property "BRIEF"
                    (read-string "Set brief: "
                                 (xroam-get-prop "BRIEF"))))

(defun xroam-add-prop (key values)
  "Set a property with VALUES (list).
This will stringify the list; trim; remove
excess spaces; sort; make values unique."
  (org-set-property
   key
   (xorg-stringify-prop
    (seq-sort #'string> (seq-uniq (append (xroam-get-prop key) values))))))

(defun xroam-get-prop (key)
  "Return the prop value for KEY as a list.
This function removes \"\", so if your value
has that in it, it's gone."
  (mapcar (lambda (s) (s-trim (replace-regexp-in-string "\"" "" s)))
          (s-split
           "\" +\"" (cdr
                     (assoc
                      key (org-roam-node-properties
                           (org-roam-node-at-point)))))))

(defun xroam-rename-file (from to)
  "Rename an Org-Roam file. This does not namify the
TO path; it does not check if the file exists first;
it only keeps the database in sync to prevent
'UNIQUE constraint failed' errors."
  (copy-file from to nil t t t)
  (org-roam-db-autosync--rename-file-a from to)
  (delete-file from))

(defun xroam-path (&optional slug)
  "Return the full path to SLUG within
the org-roam-directory; or return just
the roam directory path."
  (f-expand (f-join org-roam-directory (or slug ""))))

(defun xroam-new-fpath (name &optional subdir)
  "Given a SUBDIR (article, concept ..) and file
NAME, return a full, absolute, and unique path.
Use this for capture template names."
  (xname-new (xroam-path subdir) (format "%s.org" name) t))

(defun xroam-rename-all ()
  "Rename all .org files in your org-roam-directory,
to a slug (file name) that represents their titles."
  (interactive)
  (xname-change-all (xname--fmatch org-roam-directory "\\.org$")
                    #'org-get-title
                    #'xroam-rename-file
                    #'f-exists-p
                    t
                    "/tmp/xname-roam"))
