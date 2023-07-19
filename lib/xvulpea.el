(setq xvulpea--typical-body "
* meta
* summary
* details
%?
* ideas
* thoughts
* conclusion
")

(defun xvulpea--article-body (&optional preview-url cover-block)
  (let* ((head "* details\n%?\n* media\n"))
    (unless (or (eq cover-block nil) (string-blank-p cover-block))
      (setf head (concat head (format "%s\n\n" cover-block))))
    (unless (or (eq preview-url nil) (string-blank-p preview-url))
      (setf head (concat head (org-link-make-string preview-url "Preview"))))
    head))

(defun xvulpea-node-find-split ()
  "Perform a Roam node find, but open the buffer in a split."
  (interactive)
  (xvulpea-node-find t))

(defun xvulpea-capture-prompt (node)
  "Prompt the user to choose a capture template.
NODE is a Roam node."
  (funcall (nth 0 (smenu "Template type" my/capture-switch)) node))

(cl-defun xvulpea-node-find (&optional other-window initial-input filter-fn pred &key templates)
  (interactive current-prefix-arg)
  (let ((node (org-roam-node-read initial-input filter-fn pred))) ; fuzzy find
    (if (org-roam-node-file node) ; if found
        (org-roam-node-visit node other-window) ; use
      (xvulpea-capture-prompt node)))) ; create

(cl-defun xvulpea-node-insert (&optional filter-fn)
  "Identical in behaviour to `org-roam-node-insert` except that it
uses the Vulpea capture templates instead.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out."
  (interactive)
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    ;; get markers from active region
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text (org-link-display-format
                                       (buffer-substring-no-properties beg end)))))
               (node (org-roam-node-read region-text filter-fn)) ; find node with text
               (description (or region-text (org-roam-node-formatted node))))
          (if (org-roam-node-id node)
              (progn
                (when region-text ;; if we had any text highlighted
                  (delete-region beg end) ;; remove it and reset markers
                  (set-marker beg nil)
                  (set-marker end nil))
                (let ((id (org-roam-node-id node)))
                  (insert (org-link-make-string (concat "id:" id) description))
                  (run-hook-with-args 'org-roam-post-node-insert-hook id description)))
            (xvulpea-capture-prompt node))))
    (deactivate-mark))) ; always deactivate

(defun xvulpea--period (year)
  "Turn a YEAR number|string (e.g. 1919) into a period
string (e.g. 1910s)."
  (format "%ds" (xtime-decade
                 (if (cl-typep year 'integer)
                     year
                   (cl-parse-integer year)))))

(defun xvulpea--make-props (htable)
  (sort
   (seq-filter
    (lambda (a-cons)
      (let* ((v (cdr a-cons)))
        ;; allow truthy strings, numbers, and t
        (cond ((stringp v) (xstr-t v))
              ((numberp v) t)
              ((eq t v) t)
              nil)))
    (ht-map (lambda (k v)
              (cons
               (upcase (s-snake-case (symbol-name k))) ; symbol => prop name
               (xorg-stringify-prop v))) ; prop values
            htable))
   (lambda (a b) (string< (car a) (car b)))))

(defun xvulpea--tagify (tags)
  "Turns a string or a list of strings, into a list
of tags -- suitable for use in Vulpea :properties.

Returns a list of tagified strings:
  e.g. (\"foo_bar\" ...)"
  (mapcar #'xtag-tagify (xseq-listify tags)))

(defun xvulpea--tv-meta-defaults (htable)
  "Use this function to set file specific properties
derived from an xtv HTABLE. This function assumes all
necessary values are set. Use xvulpea--default-tv to
do that.

This will change keys, and set values that are suitable
for Roam file properties.

It returns the modified HTABLE (for convenience)."
  (let* ((result (ht-copy htable)))
    (ht-set result 'note-type "article")
    (xht-default 'note-category (xtag-single "TV category" t) result)
    (xht-default 'genres (xtag-multi "TV genres" t) result)
    (xht-default 'actors (xtag-multi "TV actors" t) result)
    (xht-default 'writers (xtag-multi "TV writers" t) result)
    (xht-default 'directors (xtag-multi "TV directors" t) result)
    (xht-default 'year (lambda () (xprompt-year nil t)) result)
    (xht-derive result 'year 'period (lambda (year) (xvulpea--period year)))
    (xht-default 'imdb-id (lambda () (xprompt-imdb-id "IMDb ID" t)) result)
    (xht-derive result 'imdb-id 'info-url (lambda (id) (xurl-imdb id)))
    (xht-default 'state (xtag-single "Watched state" t) result)
    (when (xht= 'state "done" result)
      (xht-default 'rating (lambda () (xprompt-rating "Your rating" t)) result))
    (xht-default 'view-url (lambda () (xprompt-yt "Trailer URL (YouTube)" t)) result)
    (xht-default 'contexts (xtag-multi "TV contexts" t) result)
    result))

(defun xvulpea-tag-save-tv (htable)
  "Save TV gere, actor, irector, writer, category
and context tags."
  (xtag-write "tv-genre" (ht-get htable 'genres))
  (xtag-write "tv-actor" (ht-get htable 'actors))
  (xtag-write "tv-director" (ht-get htable 'directors))
  (xtag-write "tv-writer" (ht-get htable 'writers))
  (xtag-write "tv-category" (ht-get htable 'note-category))
  (xtag-write "tv-context" (ht-get htable 'contexts)))

(defun xvulpea--default-tv (htable title)
  "Use this functon to set default values to
to an xtv hash table (HTABLE).

TITLE is the default title to use if no item was
found. This is usually the node title.

It returns the modified HTABLE (for convenience)."
  (unless htable (setq htable (ht))) ; if nil
  htable)

(defun xvulpea--get-tv (title)
  "A convenience function that will fetch the TV props
via xtv; then default the values (prompting where
necessary); and lastly it will mutate the xtv hash table
to reflect the desired ORG properties -- where the keys
(symbols) form the property names.

TITLE: the title of the TV media to fetch from OMDb."
  (xvulpea--tv-meta-defaults
   (xvulpea--default-tv
    (xtv-prompt title) title)))

(defun xvulpea--tagify-meta (htable tags &rest keys)
  "Take an HTABLE of properties and use KEYS to access
their values. Merge all of those values, and TAGS int
values, and merge all of them into one tagified list
with duplicates removed. In addition to that, also
include normal TAGS.

Use this to extract tags from an HTABLE DTO (like xtv).
The use-case for this is to extract values from would
be file properties, into valid Roam tags."
  (let* ((result tags)
         (item nil))
    (dolist (key keys)
      (setq item (ht-get htable key))
      (if (and (cl-typep item 'seq ) (not (cl-typep item 'string))) ; str is seq too
          (setq result (append result item)) ; seq
        (setq result (add-to-list 'result item)))) ; other
    (seq-uniq (sort (xvulpea--tagify result) #'string<))))

(defun xvulpea--article-meta-defaults (htable)
  "Use this function to set properties not set on HTABLE."
  (let* ((result (ht-copy htable)))
    (ht-set result 'note-type "article")
    (xht-default 'roam-aliases (xprompt-aliases nil t) result)
    (xht-default 'note-category (xtag-single "Article category" t) result)
    (when (xtag-exists-p "needs-cover" (ht-get result 'note-category))
      (xht-default 'view-url (lambda () (xprompt-url "View URL")) result))
    (when (xtag-exists-p "needs-rating" (ht-get result 'note-category))
      (xht-default 'rating (lambda () (xprompt-rating "Your rating")) result))
    (when (xtag-exists-p "needs-year" (ht-get result 'note-category))
      (xht-default 'year (lambda () (xprompt-year "Year")) result))
    (when (xtag-exists-p "needs-state" (ht-get result 'note-category))
      (xht-default 'state (xtag-single "State" t t) result))
    (when (xtag-exists-p "needs-url" (ht-get result 'note-category))
      (xht-default 'roam-refs (lambda () (xprompt-url "Entity URL" t)) result))
    result))

(defun xvulpea--song-meta-defaults (htable)
  "Use this function to set properties not set on HTABLE."
  (let* ((result (ht-copy htable)))
    (ht-set result 'note-type "article")
    (ht-set result 'note-category "song")
    (xht-default 'view-url (lambda () (xprompt-url "Video URL" t)) result)
    (xht-derive result 'view-url 'stream-url ;; must be YouTube or a file to stream with MPV
                (lambda (url) (if (xurl-yt? url) url (xprompt-url "Stream URL (file)" t))))
    (xht-default 'artists (xtag-multi "Music artists" t) result)
    (xht-default 'genres (xtag-multi "Music genres" t) result)
    (xht-default 'contexts (xtag-multi "Music contexts" t) result)
    (xht-default 'rating (lambda () (xprompt-rating "Your rating" t)) result)
    (xht-default 'year (lambda () (xprompt-year "Year published" t)) result)
    (xht-derive result 'year 'period (lambda (year) (xvulpea--period year)))
    (xht-default 'license (lambda () (xlicense-choose)) result)
    (when (xlicense-commercial-use? (ht-get result 'license))
      (xht-default 'download-url (lambda () (xprompt-url "Download URL")) result)
      (xht-default 'info-url (lambda () (xprompt-url "Info page URL")) result))
    result))

(defun xvulpea--game-meta-defaults (htable)
  "Use this function to set properties not set on HTABLE."
  (let* ((result (ht-copy htable)))
    (ht-set result 'note-type "article")
    (ht-set result 'note-category "game")
    (xht-default 'genres (xtag-multi "Game genres" t) result)
    (xht-default 'publisher (xtag-single "Game publisher" t) result)
    (xht-default 'developer (xtag-single "Game developer" t) result)
    (xht-default 'view-url (lambda () (xprompt-yt "Trailer URL (YouTube)" t)) result)
    (xht-default 'year (lambda () (xprompt-year "Year published (or expected)" t)) result)
    (xht-derive result 'year 'period (lambda (year) (xvulpea--period year)))
    (cond ((xtime-year-p (ht-get result 'year) '=)
           (ht-set result 'release-state (xtag-single "Release state" nil t)))
          ((xtime-year-p (ht-get result 'year) '>)
           (ht-set result 'release-state "unreleased"))
          ((xtime-year-p (ht-get result 'year) '<)
           (ht-set result 'release-state "released")))
    (ht-set result 'state (xtag-single "Played state"))
    (when (string= (ht-get result 'state) "done")
      (xht-default 'rating (lambda () (xprompt-rating "Your rating")) result))
    result))

(defun xvulpea--concept-meta-defaults (htable)
  "Use this to set properties not set on HTABLE."
  (let* ((result (ht-copy htable)))
    (ht-set result 'note-type "concept")
    (xht-default 'roam-aliases (xprompt-aliases nil t) result)
    result))

(defun xvulpea--idea-meta-defaults (htable)
  "Use this to set properties not set on HTABLE."
  (let* ((result (ht-copy htable)))
    (ht-set result 'note-type "idea")
    (xht-default 'roam-aliases (xprompt-aliases nil t) result)
    (xht-default 'note-category (xtag-single "Type of idea" t) result)
    (when (string= (ht-get result 'note-category) "project")
      (xht-default 'project-type (xtag-single "Type of project" t) result))
    result))

(defun xvulpea--person-meta-defaults (htable)
  "Use this to set properties not set on HTABLE."
  (let* ((result (ht-copy htable)))
    (ht-set result 'note-type "person")
    (xht-default 'roam-aliases (xprompt-aliases nil t) result)
    (xht-default 'note-category (xtag-single "Type of person" t) result)
    result))

(defun xvulpea--quote-meta-defaults (htable)
  "Use this to set properties not set on HTABLE."
  (let* ((result (ht-copy htable)))
    (ht-set result 'note-type "quote")
    (xht-default 'roam-aliases (xprompt-aliases nil t) result)
    (xht-default 'note-category (xtag-single "Type of quote" t) result)
    ;; only set URL. We set cite key in "body" func: Source: [[cite:...]]
    (unless (string= (ht-get result 'note-category) "literature")
      (xht-default 'roam-refs (lambda () (xprompt-url "Quote URL" t)) result))
    result))

(setq xvulpea--quote-upper-body-t "
* meta
* summary
* details
#+begin_quote
%?
#+end_quote

")

(defun xvulpea--quote-body (&optional url)
  (let* ((upper xvulpea--quote-upper-body-t)
         (src "Source: %s\n\n")
         (lower
          (format src
                  (if (xnil-or-blank url)
                      (org-link-make-string "cite:&${my/pick-bibtex-key}") ; cite
                    (org-link-make-string url (xstr-default (xprompt "Source name") "link"))))))
    (concat upper lower)))
