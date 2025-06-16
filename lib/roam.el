;; -*- lexical-binding: t; -*-
(defvar xroam--props-tracked (xht-from-lists
                              '((actors        (custom  multi))
                                (roam-aliases) (builtin multi)
                                (brief         (custom  single))
                                (contexts      (custom  multi))
                                (cover         (custom  single))
                                (developer     (custom  single))
                                (directors     (custom  multi))
                                (download-url  (custom  single))
                                (genres        (custom  multi))
                                (imdb-id       (custom  single))
                                (imdb-rating   (custom  single))
                                (info-url      (custom  single))
                                (license       (custom  single))
                                (maturity      (custom  single))
                                (meta          (custom  single))
                                (metascore     (custom  single))
                                (project-type  (custom  single))
                                (release-state (custom  single))
                                (note-category (custom  single))
                                (note-type     (custom  single))
                                (period        (custom  single))
                                (plot          (custom  single))
                                (publisher     (custom  single))
                                (rating        (custom  single))
                                (roam-aliases  (custom  multi))
                                (state         (custom  single))
                                (stream-url    (custom  single))
                                (writers       (custom  multi))
                                (year          (custom  single))
                                (view-url      (custom  single))))
  "These are all of the properties that I track (they have configuration values).
The CDR is the kind of value that the corresponding Org property holds --
either single or multi (meaning a single value, or a list of values).")

(defvar xroam--props-taggable '(actors developer directors genres license
                                project-type release-state note-category
                                period publisher state writers year)
  "Symbols that should map to property keys,
which should be extracted, tagified, and inserted
as Roam tags.")

;; TODO: use a macro
(defun xroam-prop-set-aliases-root () (interactive) (xroam--prop-set-text 'roam-aliases t))
(defun xroam-prop-set-aliases-closest () (interactive) (xroam--prop-set-text 'roam-aliases))
(defun xroam-prop-set-brief-root () (interactive) (xroam--prop-set-text 'brief t))
(defun xroam-prop-set-brief-closest () (interactive) (xroam--prop-set-text 'brief))
(defun xroam-prop-set-meta-root () (interactive) (xroam--prop-set-text 'meta t))
(defun xroam-prop-set-meta-closest () (interactive) (xroam--prop-set-text 'meta))

(defun xroam--prop-set-text (key &optional root)
  "Set a plain old text value like ROAM_ALIASES, or META.
Do NOT set complex props with this: e.g. GENRES."
  (interactive)
  (org-with-point-at (if root (point-min) (point))
    (xroam--prop-set key
                     (org-roam-node-at-point)
                     (lambda (curr)
                       (read-string (format "Set %s: " (xorg--sym2pkey key)) curr)))))

(defun xroam--prop-get-text (key node)
  "Given a property KEY for NODE, return the property
value as a string. For 'multi forms return a semi-colon
delineated string."
  (let* ((val (xroam--prop-tracked-get key node)))
    (cond ((eq nil val) nil)
          ((stringp val) val)
          ((seqp val) (s-join "; " val))
          (t (error "Unknown type for prop value (val: %s; type: %s)" val (type-of val))))))

(cl-defun xroam--prop-set (key node cb)
  "Set the property at KEY for NODE with the value
returned from CB. CB gets one argument, the current value
as a string: for 'multi this means a semi-colon delineated
string. It MUST return a new value."
  (xroam--prop-tracked-set key
                           (funcall cb (xroam--prop-get-text key node))
                           (org-roam-node-at-point)
                           t))

;; TODO: confirm data after fetching by title
;; TODO: sort field data
(defun xroam--props-tv-refresh ()
  (org-with-point-at (point-min)
    (let* ((id (xroam--prop-tracked-get 'imdb-id (org-roam-node-at-point)))
           (title (org-get-title))
           (cmds '((rename category note-category) ; mutate xtv props into org props
                   (delete title))))
      (when (and (xnil-or-blank id) (xnil-or-blank title))
        (error "Neither the IMDB_ID or TITLE is set, cannot refresh."))
      (xroam--tagify-props
       (xvulpea-tag-save-tv
        (xroam--props-refresh
         (if id
             (lambda () (xht-mutate (xtv-get-by-id id) cmds))
           (lambda () (xht-mutate (xtv-get-by-name title) cmds)))
         #'xvulpea--tv-meta-defaults
         (org-roam-node-at-point)))))))

(defun xroam-props-refresh ()
  (interactive)
  (let* ((cat (xroam-note-category)))
    (cond ((xtag-p 'tv-category cat) (xroam--props-tv-refresh))
          (t (error "Unhandled note category: %s" cat)))))

(defun xroam-note-category (&optional node)
  "Get the NOTE_CATEGORY, or set it if it doesn't exist.
NODE is an optional Roam node -- by default it's the node
at point.

Returns the string value of NOTE_CATEGORY."
  (interactive)
  (unless node (setq node (org-roam-node-at-point)))
  (let* ((cat (xroam--prop-tracked-get 'note-category node)))
    (if cat
        cat
      (setq cat (xtag-single 'note-category "Set note category"))
      (unless cat (error "You must set a NOTE_CATEGORY"))
      (xroam--prop-tracked-set 'note-category cat node)
      cat)))

(defun xroam--tagify-props (htable &optional taggable merge)
  "For each key in TAGGABLE, extract values from the
corresponding property HTABLE, and apply them as
Roam tags.

TAGGABLE: a list of keys that correspond to Org properties.
If it's nil, then it will use the global xroam--props-taggable.

MERGE: merge TAGGABLE with xroam--props-taggable to allow you
to tag global keys, and scenario specific keys."
  (org-roam-tag-add
   (seq-filter #'stringp ;; remove nils
               (flatten-list ;; denest lists
                (mapcar (lambda (key)
                          (let* ((v (ht-get htable key)))
                            (cond ((stringp v) (xtag-tagify v))
                                  ((seqp v) (mapcar #'xtag-tagify v)) ;; will nest lists
                                  (t v))))
                        (cond ((and taggable merge)
                               (append xroam--props-taggable taggable))
                              (taggable taggable)
                              (t xroam--props-taggable)))))))

(defun xroam--tagify-props-existing (node &optional keys merge)
  "The same as xroam--tagify-props except it uses the
vales that exist within the file, instead of a hash table.

NODE is the Roam node.

KEYS is a list of symbols that correspond to Org properties.
You don't need to provide this; this is for cases where you
want to exclusively use those keys, or combine them with the
globally defined set (xroam--props-taggable).

MERGE, if t it will merge KEYS with the global set."
  (xroam--tagify-props (xroam--props-get-ht node t) keys merge))

(defun xroam--prop-tracked-p (key)
  "Check if an ORG prop is tracked (has configuration values).

KEY can be an uppercase/lowercase string; or a symbol.
It applies xorg--pkey2sym, so you can use this flexibly
to check property keys, or their symbol representation.

Returns t if it is; nil otherwise."
  (ht-contains-p xroam--props-tracked (xorg--pkey2sym key)))

(defun xroam--props-get-tracked (node)
  "Return a list of properties tracked properties -- properties
that have configuration values.

Returns an alist '((\"PROP_NAME\" . \"value\") ...)"
  (seq-filter
   (lambda (item) (xroam--prop-tracked-p (car item)))
   (org-roam-node-properties node)))

(defun xroam--prop-form (prop-key)
  "Get the property form for the given
PROP-KEY. PROP-KEY can be a property key
or a symbol:
  PROP_KEY
  prop-key

It returns a symbol: 'single or 'multi"
  (xroam--prop-conf-get prop-key 1))

(defun xroam--prop-type (prop-key)
  "Get the property type for the given
PROP-KEY. PROP-KEY can be a property key
or a symbol:
  PROP_KEY
  prop-key

It returns a symbol: 'custom or 'builtin"
  (xroam--prop-conf-get prop-key 0))

(defun xroam--prop-conf-get (prop-key n)
  "From PROP-KEY in xroam--props-tracked, get Nth
config value -- e.g. type, form."
  (nth n
       (ht-get xroam--props-tracked
               (if (stringp prop-key)
                   (xorg--pkey2sym prop-key)
                 prop-key))))

(defun xroam--prop-type= (prop-key type)
  "Check if PROP-KEY (a symbol or string)
matches TYPE (a symbol: 'custom; 'builtin).

  (xroam--prop-form= \"MY_PROP\" 'custom)
  (xroam--prop-form= 'my-prop 'builtin)"
  (eq type (xroam--prop-type prop-key)))

(defun xroam--prop-form= (prop-key form)
  "Check if PROP-KEY (a symbol or string)
matches FORM (a symbol: 'single; 'multi).

  (xroam--prop-form= \"MY_PROP\" 'single)
  (xroam--prop-form= 'my-prop 'multi)"
  (eq form (xroam--prop-form prop-key)))

(defun xroam-tag-list (&optional tags)
  "Completing-read-multiple for all roam tags + TAGS that
you wish to statically include."
  (append tags
          (completing-read-multiple "Roam tags: " (org-roam-tag-completions))))

;; DEPRECATED: unused
(defun xroam-add-prop (key values)
  "Set a property with VALUES (list).
This will stringify the list; trim; remove
excess spaces; sort; make values unique."
  (org-set-property
   key
   (xorg-stringify-prop
    (seq-sort #'string> (seq-uniq (append (xroam-get-prop key) values))))))

;; DEPRECATED: unused
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

(defun xroam-path-definitions ()
  "Return the path for word definitions."
  (xroam-path "definition"))

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

(defun xorg--pkey2sym (key)
  "Turn a property KEY into a symbol:
    FOO_BAR   => foo-bar
    FOO__BAR  => foo--bar
  ' FOO_BAR ' => foo-bar"
  (cond ((stringp key)
         (intern (downcase (replace-regexp-in-string "_" "-" (s-trim key)))))
        ((symbolp key) key)
        (t (error "Cannot convert to symbol: %s" key))))

(defun xorg--sym2pkey (sym)
  "Turn a symbol into a property key:
  'foo-bar  => FOO_BAR
  'foo--bar => FOO__BAR"
  (upcase (replace-regexp-in-string "-" "_" (symbol-name sym))))

(defun xstr-trim-quotes (str)
  (replace-regexp-in-string "\\(^\"\\|\"$\\)" "" str))

(defun xroam--split-prop (val)
  (seq-filter #'xstr-t (mapcar #'xstr-neat (split-string-and-unquote val))))

(defun xroam--prop-value-parse (val form)
  "Parse VAL into a value appropriate for FORM.

When FORM is 'single, return a string;
When FORM is 'multi, return a list of strings."
  (cond ((eq 'multi form) (xroam--split-prop val))
        ((eq 'single form) (xstr-neat (xstr-trim-quotes val)))
        (t (error "Unhandled prop form: %s" (symbol-value form)))))

(defun xroam--props-get (&optional node symbols)
  "Get the props from NODE in the form of:
  '(('key1 \"string\")                   ; FORM is 'single
    ('key2 '(\"string1\" \"string2\")))  ; FORM is 'multi

It will split strings on \"quoted words\" into a list for
'multi FORM; and strip leading and trailing \"quotes\" for
'single FORM. It will also make all strings xstr-neat.

NODE is the target Org-Roam node.
SYMBOLS means to return the prop keys as 'symbols, instead
of strings."
  (mapcar
   (lambda (prop)
     (let ((key (if symbols (xorg--pkey2sym (car prop)) (car prop)))
           (value (xroam--prop-value-parse (cdr prop) ; cdr is always a single string
                                           (xroam--prop-form (car prop)))))
       (list key value)))
   (xroam--props-get-tracked node))) ;; getting all props will break above prop-form check

(defun xroam--props-get-ht (node &optional symbols)
  (xht-from-lists (xroam--props-get node symbols)))

(defun xroam--prop-tracked-get (key node)
  "Fetch a property value from NODE given its KEY (string|symbol).

Returns a list or a string, depending on the property form."
  (when (symbolp key) (setq key (xorg--sym2pkey key)))
  (let* ((pair (seq-filter
                (lambda (item) (string= key (car item)))
                (xroam--props-get-tracked node))))
    (cond ((length= pair 0) nil)
          ((length= pair 1)
           (xroam--prop-value-parse
            (cdr (car pair))
            (xroam--prop-form (car (car pair)))))
          (t (error "More than one property for key: %s" key)))))

(defun xroam--prop-quotify (str)
  "Turn ' foo ; bar   baz  ' => 'foo \"bar baz\"'."
  (combine-and-quote-strings (xstr-neat (s-split "; *" str))))

(defun xroam--prop-tracked-set (key value node &optional replace)
  "Replace an existing prop with VALUE, given its KEY and roam NODE.
For 'multi props, it will quotify (split on ;); for 'single it will
set the value as-is.

For 'multi props, if REPLACE is t, it will replace the value,
otherwise it will append it."
  (when (symbolp key) (setq key (xorg--sym2pkey key)))
  (let* ((form (xroam--prop-form key))
         (pnt (org-roam-node-point node)))
    (cond ((xeq form 'single) (org-entry-put pnt key value))
          ((xeq form 'multi)
           (if replace
               (if (stringp value)
                   (org-entry-put pnt key (xroam--prop-quotify value))
                 (org-entry-put pnt key (combine-and-quote-strings value)))
             (org-with-point-at pnt
               (if (stringp value)
                   (org-roam-property-add key (xstr-neat value))
                 (seq-doseq (v value)
                   (org-roam-property-add key (xstr-neat v)))))))
          (t (error "Unhandled property form (key: %s; form: %s)" key form)))))

(setq xroam--prop-msg-conflict "Value conflict for '%s' -- choose one:

\tCurrent: %s
\t\tNew: %s

")

(defun xroam--prompt-new-curr (key new curr)
  (let* ((choice (car (smenu (format xroam--prop-msg-conflict (xorg--sym2pkey key) curr new)
                             '((?c "current" current) (?n "new" new))))))
    (when (eq choice nil) (throw 'menu-quit "The user quit the menu."))
    choice))

(defun xroam--prompt-prop-conflict (key new curr)
  (let* ((choice (xroam--prompt-new-curr key new curr))) ; different, so pick
    (cond ((eq choice 'new) new)
          ((eq choice 'current) curr)
          (t (error "Invalid choice: %s" choice)))))

(defun xroam--props-merge-one (key new curr &optional prompter)
  " NEW|CURR is a list of strings, or a string.
PROMPTER is a function: (KEY NEW CURR) => value
(returns the chosen value)"
  (cond ((xroam--prop-form= key 'single)
         (unless (or (eq new nil) (stringp new)) (setq new (xstr new)))
         ;; checking for nil nil means we have to prompt for something, but what?
         ;; we will do defaulting after this, so returning nil is okay.
         (cond ((eq new nil) curr) ; might return nil:
               ((eq curr nil) new) ;   in that case both are nil
               ((xeq new curr) curr) ; neither are nil here
               (t (if prompter  ; different; not nil
                      (funcall prompter key new curr)
                    (xroam--prompt-prop-conflict key new curr)))))
        ((xroam--prop-form= key 'multi)
         (xlist-insert-alpha curr new))
        (t (error "Unknown tracked prop form (key: %s form: %s)" key (xroam--prop-form key)))))


(defun xroam--props-merge (new curr &optional prompter)
  (let* ((result (ht)))
    (seq-doseq (k (xht-keys new curr))
      (ht-set result k (xroam--props-merge-one k
                                               (ht-get new k) ; list or string
                                               (ht-get curr k) ; list or string
                                               prompter)))
    result))

(defun xroam--props-merge-all (new-fn defaults-fn node &optional prompter)
  (funcall defaults-fn
           (xroam--props-merge (funcall new-fn)
                               (xht-from-lists (xroam--props-get node t))
                               prompter)))

;; TODO: only when Roam mode
(defun xroam--props-refresh (new-fn defaults-fn node &optional prompter)
  "Apply the product or merging new props, with current, and defaults.
Returns a hash table."
  (let* ((result (ht)))
    (maphash (lambda (k v)
               (org-entry-put
                (org-roam-node-point node)
                (xorg--sym2pkey k)
                (cond ((xroam--prop-form= k 'single)
                       (let* ((val (xstr-neat v)))
                         (if (string-match " " val) (format "\"%s\"" val) val))) ; quote if has spaces
                      ((xroam--prop-form= k 'multi)
                       (combine-and-quote-strings (xstr-neat v)))
                      (t
                       (error "Unknown prop form %s" (xroam--prop-form k))))))
             (setq result (xroam--props-merge-all new-fn defaults-fn node prompter)))
    result))

(defun xroam--status-tags (node)
  "Get a list of status tags for NODE."
  (seq-filter (lambda (el) (xhas '("todo" "in_progress") el))
              (org-roam-node-tags node)))

(defun xroam-tag-replace (old new &optional node)
  "Replace OLD tag with NEW for NODE (defaults to root node)."
  (let* ((pos (if node (org-roam-node-point node) 1)))
    (org-with-point-at pos
      (org-roam-tag-remove (list old))
      (org-roam-tag-add (list new)))))

(defun xroam-tag-add (tags &optional node)
  "Add TAGS to node -- can be a string or list of strings."
  (let* ((pos (if node (org-roam-node-point node) 1)))
    (org-with-point-at pos
      (org-roam-tag-add (if (stringp tags) (list tags) tags)))))

(defun xroam--status-toggle (&optional node)
  "Toggle the status for NODE."
  (let* ((node (org-roam-node-at-point))
         (tags (xroam--status-tags node)))
    (cond ((xhas tags "todo")        (xroam-tag-replace "todo" "in_progress" node))
          ((xhas tags "in_progress") (xroam-tag-replace "in_progress" "done" node))
          ((xhas tags "done")        (xroam-tag-replace "done" "todo" node))
          (t (xroam-tag-add "todo" node))))
  (save-buffer))

(defun xroam-status-toggle-root ()
  "Toggle status for the root node."
  (interactive)
  (org-with-point-at 1
    (xroam--status-toggle (org-roam-node-at-point))))

(defun xroam-status-toggle-child ()
  "Toggle status for a nested node."
  (interactive)
  (xroam--status-toggle (org-roam-node-at-point)))

(defun xroam-cache-refresh ()
  "Refresh the completions cache"
  (interactive)
  (setq xroam--cache nil)
  (org-roam-node-read--completions))

(defvar xroam--cache nil "A memory cache for node find completions")
(advice-add #'org-roam-node-read--completions
            :around
            (lambda (fn &rest args)
              (when (not xroam--cache)
                (setq xroam--cache (apply fn args)))
              xroam--cache))

(defun xroam-node= (a b)
  "Test that two Roam nodes are equal via their ID."
  (string= (org-roam-node-id a) (org-roam-node-id b)))

(defun xroam--completion= (node comp)
  "Test that a completion entry COMP matches a NODE.
  COMP is (title . node) returned used in `org-roam-node-read--completions`."
  (xroam-node= node (cdr comp)))

(defun xroam--comp-has-path (path comp)
  "Test that a node within a completion COMP matches a PATH,
  where a completion is (title . node)."
  (string= path (org-roam-node-file (cdr comp))))

(defun xroam-node-create-at-path (path title)
  "Create an org-roam node at PATH with TITLE.

PATH should be relative to the 'org-roam' directory, e.g., foo/bar."
  (let ((fpath (expand-file-name path org-roam-directory)))
    (make-directory (file-name-directory fpath) t)
    (with-temp-file fpath
      (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n#+title: %s\n\n" (org-id-uuid) title)))
    (org-id-update-id-locations '(fpath))
    (org-roam-db-update-file fpath)
    fpath))

;; Note the smartest clone, but `copy-org-roam-node` doesn't exist anywhere.
(defun xroam--node-clone (node title)
  "A brittle clone of a Roam node. Do not rely on this to save
  for data integrity -- the properties may not be correct. This is
  used to create duplications in completions -- i.e. the same node
  with different aliases. This isn't data critical, and as it stands
  now, it's okay if this structure goes out of sync with upstream."
  (org-roam-node-create :id (org-roam-node-id node)
                        :file (org-roam-node-file node)
                        :file-title title
                        :file-atime (org-roam-node-file-atime node)
                        :file-mtime (org-roam-node-file-mtime node)
                        :level (org-roam-node-level node)
                        :point (org-roam-node-point node)
                        :todo (org-roam-node-todo node)
                        :priority (org-roam-node-priority node)
                        :scheduled (org-roam-node-scheduled node)
                        :deadline (org-roam-node-deadline node)
                        :title  title
                        :aliases (org-roam-node-aliases node)
                        :properties (org-roam-node-properties node)
                        :olp (org-roam-node-olp node)
                        :tags (org-roam-node-tags node)
                        :refs (org-roam-node-refs node)))

(defun xroam--cache-add (node)
  "Add a node to the memory cache."
  (org-roam-node-read--completions)
  (setq xroam--cache (cl-remove node xroam--cache :test #'xroam--completion=))
  (let* ((template (org-roam-node--process-display-format
                    org-roam-node-display-template)))
    (dolist (alias (org-roam-node-aliases node))
      (let* ((new (xroam--node-clone node alias))
             (candidate (org-roam-node-read--to-candidate new template)))
        (push candidate xroam--cache)))
    (push (org-roam-node-read--to-candidate node template) xroam--cache)))

(defun xroam--cache-remove (path)
  "Remove all nodes with the PATH from the memory cache."
  (org-roam-node-read--completions)
  (setq xroam--cache (cl-remove path xroam--cache :test #'xroam--comp-has-path)))

;; add files to memory cache when created, modified, or renamed.
(advice-add 'org-roam-db-insert-file-node
            :after
            (lambda (&rest args)
              (org-with-point-at 1 (xroam--cache-add (org-roam-node-at-point)))))

;; remove files from memory cache when deleted.
(advice-add 'org-roam-db-autosync--delete-file-a
            :after
            (lambda (file &optional trash)
              (when (org-roam-file-p file)
                (xroam--cache-remove file))))
