(require 'yaml)

(defun xlicense--get ()
  "Returns a hashmap of the licenses file.

If license variable isn't bound, load it from
file then bind it first."
  (if (boundp 'xlicense--hmap)
      xlicense--hmap
    (setq xlicense--hmap
          (yaml-parse-string
           (my/read-file xlicense-fpath)
           :sequence-type 'list
           :object-key-type 'string))
    xlicense--hmap))

(defun xlicense--init ()
  "Set up everything needed to function properly."
  (mkdir xlicense-dpath t)
  (makunbound 'xlicense--hmap)
  (setq xlicense--hmap (xlicense--get)))

(xlicense--init)

(defun xlicense--write()
  "Write xlicense-hmap to disk."
  (f-write
   (yaml-encode xlicense--hmap) 'utf-8 xlicense-fpath))

(defun xlicense--tags-add (license tags)
  "Add a TAG to LICENSE."
  (let* ((old (append (gethash license (xlicense--get)) ()))
         (new (seq-uniq (append old tags))))
    (puthash license new xlicense--hmap)
    (xlicense--write)))

(defun xlicense--tags-rm (license tags)
  "Remove a single TAG from LICENSE."
  (let* ((old (append (gethash license (xlicense--get)) ()))
         (new (seq-filter (lambda (v) (not (member v tags))) old)))
    (puthash license new xlicense--hmap)
    (xlicense--write)))

(defun xlicense--license-rm (license)
  "Remove a license completely."
  (remhash license xlicense--hmap)
  (xlicense--write))

(defun xlicense--tags ()
  "Return all tracked tags."
  (let* ((all-tags ()))
    (maphash
     (lambda (license some-tags)
       (setq all-tags (append all-tags some-tags)))
     xlicense--hmap)
    (sort (seq-uniq all-tags) #'string<)))

(defun xlicense--names ()
  "Return a list of tracked license names."
  (let* ((all-names ()))
    (maphash
     (lambda (name some-tags)
       (if (length= all-names 0)
           (setq all-names (list name))
         (setq all-names (add-to-list 'all-names name))))
     xlicense--hmap)
    (sort (seq-uniq all-names) #'string<)))

(defun xlicense--name-exists? (name)
  (xhas (xlicense--names) name))

(defun xlicense--tags-for (license)
  "Return a list of tags for LICENSE."
  (gethash license xlicense--hmap))

(defun xlicense-has-any-tags? (license tags)
  "Return t if LICENSE has any TAGS (a seq)."
  (xhas-any (xlicense--tags-for license) tags))

(defun xlicense-has-all-tags? (license tags)
  "Return t if LICENSE has all TAGS (a seq)."
  (xhas-all (xlicense--tags-for license) tags))

(defun xlicense-commercial-use? (license)
  (xhas-all (xlicense--tags-for license) '("commercial use")))

(defun xlicense-choose ()
  "Choose a license and return its string.
If the license does not exist, create it and
prompt to apply some tags."
  (interactive)
  (let* ((license (completing-read "Choose a license: " (xlicense--names))))
    (unless (xlicense--name-exists? license)
      (xlicense--tags-add
       license
       (completing-read-multiple "Choose license tags: " (xlicense--tags) nil t)))
    license))

(defun xlicense-tag-add ()
  "Choose a license and add tags to it.
If the license doesn't exist, create it.

Returns the license name."
  (interactive)
  (let* ((license (completing-read "Choose a license: " (xlicense--names) nil)))
    (xlicense--tags-add
     license
     (completing-read-multiple
      (format "Choose tags for '%s' license: " license)
      (xlicense--tags) nil t))
    license))

(defun xlicense-tag-rm ()
  "Choose a license and remove tags from it.

Returns the license name."
  (interactive)
  (let* ((license (completing-read "Choose a license: " (xlicense--names) nil t)))
    (xlicense--tags-rm
     license
     (completing-read-multiple
      (format "Remove tags from '%s' license: " license)
      (xlicense--tags) nil t))
    license))
