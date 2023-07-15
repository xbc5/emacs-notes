(defvar my/capture-switch)
(setq my/capture-switch '((?a "article" xvulpea--capture-article)
                          (?c "concept" xvulpea--capture-concept)
                          (?g "game" xvulpea--capture-game)
                          (?i "idea" xvulpea--capture-idea)
                          (?t "tv" xvulpea--capture-tv)
                          (?p "person" xvulpea--capture-person)
                          (?q "quote" xvulpea--capture-quote)
                          (?s "song" xvulpea--capture-song)))

(use-package! vulpea
  :after org-roam ; so that we reset keymaps
  :config
  (map! "M-n" #'xvulpea-node-find
        "M-N" #'xvulpea-node-find-split
        "M-I" #'xvulpea-node-insert)

  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))

(defun xvulpea--capture-article (node)
  (let* ((title (org-roam-node-title node))
         (meta (xvulpea--article-meta-defaults (ht))) ;; plug the API hash table in here
         (cover-block  (when (xtag-exists-p "needs-cover" (ht-get meta 'note-categoty))
                         (ximg-block-create :tag "cover" :name title :desc "Cover IMG"))))
    (vulpea-create title
                   (xroam-new-fpath title (ht-get meta 'note-type))
                   :properties (xvulpea--make-props meta)
                   :tags (xvulpea--tagify-meta meta (my/roam-tag-list) 'note-category 'state)
                   :body (xvulpea--article-body (ht-get meta 'view-url) cover-block))))

(defun xvulpea--capture-game (node)
  (let* ((title (org-roam-node-title node))
         (meta (xvulpea--game-meta-defaults (ht))) ;; plug the API hash table in here
         (cover-block (ximg-block-create :tag (ht-get meta 'note-category)
                                         :name title
                                         :desc "Cover IMG")))
    (vulpea-create title
                   (xroam-new-fpath title (ht-get meta 'note-type))
                   :properties (xvulpea--make-props meta)
                   :body (xvulpea--article-body (ht-get meta 'view-url) cover-block))))

(defun xvulpea--capture-song (node)
  (let* ((title (org-roam-node-title node))
         (meta (xvulpea--song-meta-defaults (ht))) ;; plug the API hash table in here
         (tags (my/roam-tag-list)))
    (vulpea-create title
                   (xroam-new-fpath title (ht-get meta 'note-type))
                   :properties (xvulpea--make-props meta)
                   :tags (xvulpea--tagify-meta meta tags 'note-category 'artists
                                               'genres 'contexts 'license 'period)
                   :body (xvulpea--article-body (ht-get meta 'view-url)))))

(defun xvulpea--capture-tv (node)
  (let* ((ttl (org-roam-node-title node))
         (meta (xvulpea--tv-meta-defaults (xtv-prompt ttl)))
         (title (or (xht-pluck meta 'title) ttl))
         (cover-block (if meta
                          (ximg-block (gethash 'cover meta) "Cover IMG")
                        (ximg-block-create :tag "cover" :name title :desc "Cover IMG"))))
    (xtag-write "tv-genre" (ht-get meta 'genres))
    (xtag-write "tv-actor" (ht-get meta 'actors))
    (xtag-write "tv-director" (ht-get meta 'directors))
    (xtag-write "tv-writer" (ht-get meta 'writers))
    (xtag-write "tv-category" (ht-get meta 'category))
    (xtag-write "tv-context" (ht-get meta 'contexts))
    (vulpea-create title
                   (xroam-new-fpath title (ht-get meta 'note-type))
                   :properties (xvulpea--make-props meta)
                   :tags (xvulpea--tagify-meta meta (my/roam-tag-list) 'note-category
                                               'state 'genres 'actors 'directors 'writers
                                               'contexts 'period)
                   :body (xvulpea--article-body (ht-get meta 'view-url) cover-block))))

(defun xvulpea--capture-concept (node)
  (let* ((title (org-roam-node-title node))
         (meta (xvulpea--concept-meta-defaults (ht))))
    (vulpea-create title
                   (xroam-new-fpath title (ht-get meta 'note-type))
                   :properties (xvulpea--make-props meta)
                   :tags (xvulpea--tagify-meta meta (my/roam-tag-list) 'note-category)
                   :body xvulpea--typical-body)))

(defun xvulpea--capture-idea (node)
  (let* ((title (org-roam-node-title node))
         (meta (xvulpea--idea-meta-defaults (ht)))) ;; plug API or other data here
    (vulpea-create title
                   (xroam-new-fpath title (ht-get meta 'note-type))
                   :properties (xvulpea--make-props meta)
                   :tags (xvulpea--tagify-meta
                          meta (my/roam-tag-list) 'note-category 'project-type)
                   :body xvulpea--typical-body)))

(defun xvulpea--capture-person (node)
  (let* ((title (org-roam-node-title node))
         (meta (xvulpea--person-meta-defaults (ht)))) ;; plug API or other data here
    (vulpea-create title
                   (xroam-new-fpath title (ht-get meta 'note-type))
                   :properties (xvulpea--make-props meta)
                   :tags (xvulpea--tagify-meta
                          meta (my/roam-tag-list) 'note-category)
                   :body xvulpea--typical-body)))

(defun xvulpea--capture-quote (node)
  (let* ((title (org-roam-node-title node))
         (meta (xvulpea--quote-meta-defaults (ht)))) ;; plug API or other data here
    (vulpea-create title
                   (xroam-new-fpath title (ht-get meta 'note-type))
                   :properties (xvulpea--make-props meta)
                   :tags (xvulpea--tagify-meta
                          meta (my/roam-tag-list) 'note-category)
                   :body  (xvulpea--quote-body (ht-get meta 'roam-refs)))))
