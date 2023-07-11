;; Credit to nobiot for some of these functions:
;;  https://org-roam.discourse.group/t/tips-how-to-combine-org-roam-capture-org-capture-and-other-commands-into-a-single-menu-prompt/1262
;;
;; interactive
;; (cons 1 '(2)) aka consing
;; string-match
(require 'vulpea)

(use-package! vulpea
  :after org-roam ; so that we reset keymaps
  :config
  (map! "M-n" #'my/vulpea-node-find
        "M-N" #'my/vulpea-node-find-split
        "M-I" #'my/vulpea-node-insert)

  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))

(defun my/vulpea-node-find-split ()
  "Perform a Roam node find, but open the buffer in a split."
  (interactive)
  (my/vulpea-node-find t))

(defun my/vulpea--article-body (&optional preview-url cover-block)
  (let* ((head "* details\n%?\n* media\n"))
    (unless (or (eq cover-block nil) (string-blank-p cover-block))
      (setf head (concat head (format "%s\n\n" cover-block))))
    (unless (or (eq preview-url nil) (string-blank-p preview-url))
      (setf head (concat head (org-link-make-string preview-url "Preview"))))
    head))

(setq my/vulpea--typical-body "* meta\n* summary\n* details\n%?\n* conclusion\n")

;; TODO: add message to menu: e.g. Download source:
;; TODO: add [u]se option on file conflict
;; TODO: do URL embeds
(defun my/vulpea--capture-article (node)
  (let* ((title (org-roam-node-title node))
         (aliases (xprompt-aliases))
         (cat (xtag-pick "article" "Article category"))
         (preview-url  (when (xtag-exists-p "needs-preview" cat)
                         (xprompt-url "Preview URL")))
         (cover-block  (when (xtag-exists-p "needs-cover" cat)
                         (ximg-block-create :tag "cover" :name title :desc "Cover IMG")))
         (rating  (when (xtag-exists-p "needs-rating" cat)
                    (xprompt-rating)))
         (year  (when (xtag-exists-p "needs-year" cat)
                  (xprompt-year)))
         (state  (when (xtag-exists-p "needs-state" cat)
                   (xtag-pick "state" "Article state")))
         (roamrefs  (when (xtag-exists-p "needs-url" cat)
                      (xprompt-url "Homepage URL")))
         (tags (my/roam-tag-list)))
    (vulpea-create title (xroam-new-fpath title "article")
                   :properties (my/vulpea-props :type "article"
                                                :cat cat
                                                :aliases aliases
                                                :state state
                                                :year year
                                                :rating rating
                                                :roamrefs roamrefs)
                   :tags (cons cat tags)
                   :body (my/vulpea--article-body preview-url cover-block))))

(defun my/vulpea--capture-song (node)
  (let* ((title (org-roam-node-title node))
         (view-url (xprompt-url "View URL" t)) ;; i.e. the thing that you'd browse to
         (stream-url (if (xurl-yt? view-url) view-url (xprompt-url "File URL" t))) ;; MPV can stream YT links
         (artists (xprompt-crm "Artists" "artist")) ; not required; allow new
         (genres (xprompt-crm "Genres" "music-genre"))
         (ctx (xprompt-crm "Song contexts" "song-context"))
         (tags (my/roam-tag-list))
         (rating (xprompt-rating nil t))
         (year (xprompt-year nil t))
         (license (xlicense-choose))
         (comm-use (xlicense-commercial-use? license))
         (download-url  (when comm-use (xprompt-url "Download URL" t)))
         (info-url  (when comm-use (xprompt-url "Info page URL" t))))
    (vulpea-create title (xroam-new-fpath title "article")
                   :properties
                   (my/vulpea-props :type "article"
                                    :cat "song"
                                    :state "done" ; when do you ever not listen to a song first?
                                    :year year
                                    :rating rating
                                    :artists artists
                                    :license license
                                    :genres genres
                                    :contexts ctx
                                    :download-url download-url
                                    :stream-url stream-url
                                    :view-url view-url
                                    :info-url info-url)
                   :tags (seq-uniq (append '("song")
                                           (xvulpea--tagify artists)
                                           (xvulpea--tagify genres)
                                           (xvulpea--tagify ctx)
                                           tags))
                   :body (my/vulpea--article-body view-url)))) ;; TODO

(defun my/vulpea--capture-concept (node)
  (let* ((title (org-roam-node-title node))
         (aliases (xprompt-aliases)))
    (vulpea-create title (xroam-new-fpath title "concept")
                   :properties (my/vulpea-props :type "concept"
                                                :aliases aliases)
                   :tags (my/roam-tag-list)
                   :body my/vulpea--typical-body)))

(defun my/vulpea--capture-idea (node)
  (let* ((title (org-roam-node-title node))
         (aliases (xprompt-aliases))
         (cat (xtag-pick "idea" "Type of idea"))
         (subcat (when (string= cat "project")
                   (xtag-pick "project" "Type of project")))
         (tags (list cat subcat)))
    (vulpea-create title (xroam-new-fpath title "idea")
                   :properties (my/vulpea-props :type "idea"
                                                :cat cat
                                                :aliases aliases)
                   :tags (append tags (my/roam-tag-list))
                   :body  "* meta\n* summary\n* details\n%?")))

(defun my/vulpea--capture-person (node)
  (let* ((title (org-roam-node-title node))
         (aliases (xprompt-aliases))
         (cat (xtag-pick "person" "Type of person"))
         (tags (my/roam-tag-list)))
    (vulpea-create title (xroam-new-fpath title "person")
                   :properties (my/vulpea-props :type "person"
                                                :cat cat
                                                :aliases aliases)
                   :tags (cons cat tags)
                   :body my/vulpea--typical-body)))

(setq my/vulpea--quote-upper-body-t "
* meta
* summary
* details
#+begin_quote
%?
#+end_quote

")

(defun my/vulpea--quote-body (&optional url)
  (let* ((upper my/vulpea--quote-upper-body-t)
         (src "Source: %s\n\n")
         (lower (if (xnil-or-blank url)
                    (org-link-make-string "cite:&${my/pick-bibtex-key}") ; cite
                  (format src (org-link-make-string
                               url (xstr-default (xprompt "Source name") "link")))))) ; url
    (concat upper lower)))

(defun my/vulpea--capture-quote (node)
  (let* ((title (org-roam-node-title node))
         (cat (xtag-pick "quote" "Type of quote"))
         (tags (my/roam-tag-list))
         (url (when (not (string= cat "literature"))
                (xprompt-url "Quote URL")))) ; not required, we prompt for a cite key otherwise
    (vulpea-create title (xroam-new-fpath title "quote")
                   :properties (my/vulpea-props :type "quote"
                                                :cat cat
                                                :roamrefs url)
                   :tags (cons cat tags)
                   :body (my/vulpea--quote-body url))))

;; credit to nobiot
(defvar my/capture-switch)
(setq my/capture-switch '((?a "article" my/vulpea--capture-article)
                          (?c "concept" my/vulpea--capture-concept)
                          (?i "idea" my/vulpea--capture-idea)
                          (?p "person" my/vulpea--capture-person)
                          (?q "quote" my/vulpea--capture-quote)
                          (?s "song" my/vulpea--capture-song)))
