;; -*- lexical_binding: t; -*-

;; TODO: confirm data after fetching by title
;; TODO: tagify added data;
;; TODO: sort field data
(defun xroam--props-tv-refresh ()
  (org-with-point-at (point-min)
    (let* ((id (xroam--prop-custom-get 'imdb-id (org-roam-node-at-point)))
           (title (org-get-title))
           (cmds '((rename category note-category) ; mutate xtv props into org props
                   (delete title))))
      (when (and (xnil-or-blank id) (xnil-or-blank title))
        (error "Neither the IMDB_ID or TITLE is set, cannot refresh."))
      (xvulpea-tag-save-tv
       (xroam--props-refresh
        (if id
            (lambda () (xht-mutate (xtv-get-by-id id) cmds))
          (lambda () (xht-mutate (xtv-get-by-name title) cmds)))
        #'xvulpea--tv-meta-defaults
        (org-roam-node-at-point))))))

(defun xroam-props-refresh ()
  (interactive)
  (let* ((cat (xroam-note-category)))
    (cond ((xtag-exists-p "tv-category" cat) (xroam--props-tv-refresh))
          (t (error "Unhandled note category: %s" cat)))))

(defun xroam-note-category (&optional node)
  "Get the NOTE_CATEGORY, or set it if it doesn't exist.
NODE is an optional Roam node -- by default it's the node
at point.

Returns the string value of NOTE_CATEGORY."
  (interactive)
  (unless node (setq node (org-roam-node-at-point)))
  (let* ((cat (xroam--prop-custom-get 'note-category node)))
    (if cat
        cat
      (setq cat (xtag-single "Set note category"))
      (unless cat (error "You must set a NOTE_CATEGORY"))
      (xroam--prop-custom-set 'note-category cat node)
      cat)))

(defvar xroam--prop-types nil
  "These are all of the custom properties that I track.
The CDR is the kind of value that the corresponding Org property holds --
either single or multi (meaning a single value, or a list of values).")

(defun xroam--prop-custom-p (key)
  "Check if an ORG prop is custom (self-made by me).

KEY can be an uppercase/lowercase string; or a symbol.
It applies xorg--pkey2sym, so you can use this flexibly
to check property keys, or their symbol representation.

Returns t if it is; nil otherwise."
  (ht-contains-p xroam--prop-types (xorg--pkey2sym key)))

(defun xroam--props-get-custom (node)
  "Return a list of properties tracks only by me --
e.g. actors, directors.

Returns an alist '((\"PROP_NAME\" . \"value\") ...)"
  (seq-filter
   (lambda (item) (xroam--prop-custom-p (car item)))
   (org-roam-node-properties node)))

(defun xroam--prop-type (prop-key)
  "Get the property type for the given
PROP-KEY. PROP-KEY can be a property key
or a symbol:
  PROP_KEY
  prop-key

It returns a symbol: 'single or 'multi"
  (ht-get xroam--prop-types
          (if (stringp prop-key)
              (xorg--pkey2sym prop-key)
            prop-key)))

(defun xroam--prop-type= (prop-key type)
  "Check if PROP-KEY (a symbol or string)
matches TYPE (a symbol: 'single; 'multi).

  (xroam--prop-type= \"MY_PROP\" 'single)
  (xroam--prop-type= 'my-prop 'multi)"
  (eq type (xroam--prop-type prop-key)))

(defun my/roam-tag-list ()
  (let ((crm-separator "[ 	]*:[ 	]*"))
    (completing-read-multiple "Roam tags: " (org-roam-tag-completions))))

(defun my/roam-set-brief ()
  (interactive)
  (org-set-property "BRIEF"
                    (read-string "Set brief: "
                                 (xroam-get-prop "BRIEF"))))

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

(defun xroam--prop-value-parse (val type)
  "Parse VAL into a value appropriate for TYPE.

When TYPE is 'single, return a string;
When TYPE is 'multi, return a list of strings."
  (cond ((eq 'multi type) (xroam--split-prop val))
        ((eq 'single type) (xstr-neat (xstr-trim-quotes val)))
        (t (error "Unhandled prop type: %s" (symbol-value type)))))

(defun xroam--props-get (&optional node symbols)
  "Get the props from NODE in the form of:
  '(('key1 \"string\")                   ; TYPE is 'single
    ('key2 '(\"string1\" \"string2\")))  ; TYPE is 'multi

It will split strings on \"quoted words\" into a list for
'multi TYPE; and strip leading and trailing \"quotes\" for
'single TYPE. It will also make all strings xstr-neat.

NODE is the target Org-Roam node.
SYMBOLS means to return the prop keys as 'symbols, instead
of strings."
  (mapcar
   (lambda (prop)
     (let ((key (if symbols (xorg--pkey2sym (car prop)) (car prop)))
           (value (xroam--prop-value-parse (cdr prop) ; cdr is always a single string
                                           (xroam--prop-type (car prop)))))
       (list key value)))
   (xroam--props-get-custom node))) ;; getting all props will break above prop-type check

(defun xroam--prop-custom-get (key node)
  "Fetch a property value from NODE given its KEY (string|symbol).

Returns a list or a string, depending on the property type."
  (when (symbolp key) (setq key (xorg--sym2pkey key)))
  (let* ((pair (seq-filter
                (lambda (item) (string= key (car item)))
                (xroam--props-get-custom node))))
    (cond ((length= pair 0) nil)
          ((length= pair 1)
           (xroam--prop-value-parse
            (cdr (car pair))
            (xroam--prop-type (car (car pair)))))
          (t (error "More than one property for key: %s" key)))))

(defun xroam--prop-quotify (str)
  "Turn ' foo ; bar   baz  ' => 'foo \"bar baz\"'."
  (combine-and-quote-strings (xstr-neat (s-split "; *" str))))

(defun xroam--prop-custom-set (key value node &optional replace)
  "Replace an existing prop with VALUE, given its KEY and roam NODE.
For 'multi props, it will quotify (split on ;); for 'single it will
set the value as-is.

For 'multi props, if REPLACE is t, it will replace the value,
otherwise it will append it."
  (when (symbolp key) (setq key (xorg--sym2pkey key)))
  (let* ((type (xroam--prop-type key))
         (pnt (org-roam-node-point node)))
    (cond ((xeq type 'single) (org-entry-put pnt key value))
          ((xeq type 'multi)
           (if replace
               (if (stringp value)
                   (org-entry-put pnt key (xroam--prop-quotify value))
                 (org-entry-put pnt key (combine-and-quote-strings value)))
             (org-with-point-at pnt
               (if (stringp value)
                   (org-roam-property-add key (xstr-neat value))
                 (seq-doseq (v value)
                   (org-roam-property-add key (xstr-neat v)))))))
          (t (error "Unhandled property type (key: %s; type: %s)" key type)))))

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
  (cond ((xroam--prop-type= key 'single)
         (unless (or (eq new nil) (stringp new)) (setq new (xstr new)))
         ;; checking for nil nil means we have to prompt for something, but what?
         ;; we will do defaulting after this, so returning nil is okay.
         (cond ((eq new nil) curr) ; might return nil:
               ((eq curr nil) new) ;   in that case both are nil
               ((xeq new curr) curr) ; neither are nil here
               (t (if prompter  ; different; not nil
                      (funcall prompter key new curr)
                    (xroam--prompt-prop-conflict key new curr)))))
        ((xroam--prop-type= key 'multi)
         (xlist-insert-alpha curr new))
        (t (error "Unknown custom prop type (key: %s type: %s)" key (xroam--prop-type key)))))


(defun xroam--props-merge (new curr &optional prompter)
  (print new)
  (print curr)
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
                (cond ((xroam--prop-type= k 'single)
                       (let* ((val (xstr-neat v)))
                          (if (string-match " " val) (format "\"%s\"" val) val))) ; quote if has spaces
                      ((xroam--prop-type= k 'multi)
                       (print (combine-and-quote-strings (xstr-neat v)))
                       (combine-and-quote-strings (xstr-neat v)))
                      (t
                       (error "Unknown prop type %s" (xroam--prop-type k))))))
             (setq result (xroam--props-merge-all new-fn defaults-fn node prompter)))
    result))

(defun xroam--props-apply (props)
  "Given an alist of PROPS, destructively apply each.

The alist has lots of quotes: quotes that compete with
their surrounding string quotes; quotes that compete with
the doc string quotes, so without spending 6 hours trying
to make it work, just accept this example:
  e.g. '((KEY 'val1 val2') (KEY2 'val3 val4'))

Each of val1 an val2 must be surrounded like \"val1\" if you
want them to be separate within the final string.

Conveniently, xvulpea--make-props returns the each format
that you need, so provide your hash table to that. "
  (seq-doseq (prop props)
    (org-set-property (car prop) (nth 1 prop))))
