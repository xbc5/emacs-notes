;; -*- lexical-binding: t; -*-
(require 'ht)

(cl-defun xtag-pick (fname msg &key match)
  "Pick a tag from {xtag--files}/{fname}, if it doesn't exist -- write it."
  (let* ((tags (xtag-get fname))
         (choice
          (replace-regexp-in-string
           " +" "_" (s-trim
                     (completing-read (concat msg ": ") tags nil match)))))
    (unless (member choice tags) (xtag-write fname choice))
    choice))

(defun xtag-tagify (tag)
  ;; xstr-neat removes excess spaces, so do lazy replace
  (replace-regexp-in-string " +?" "_" (xstr-neat tag)))

(defun xtag-exists-p (fname tag)
  "Return t if TAG exists inside tags file (FNAME)."
  (not (eq nil (member tag (xtag-get fname)))))

(defun xtag-get (name)
  "Return the tags from the given tag file as a list."
  (let ((fpath (f-join xtag--files name)))
    (mkdir xtag--files t)
    (unless (file-exists-p fpath) (f-touch fpath))
    (split-string
     (my/read-file fpath)
     "\n" t " ")))

(defun xtag-write (fname tags)
  "Write TAGS to {xtag-files}/{fname}.
TAGS can be a string, list, or nil. It will trim, sort,
and remove duplicates -- so there's no need to perform
this yourself; it will ignore nil.

FNAME doesn't need to be exact because it's run through
xtag-file first.

Returns the modified list of tags saved to file."
  (mkdir xtag--files t)
  (let* ((fname (xtag-file fname))
         (curr (xtag-get fname))
         (merged nil)
         (delta nil))
    (cond ((cl-typep tags 'string)
           (setq delta (list tags))
           (setq merged (append curr delta)))
          ((cl-typep tags 'list)
           (setq delta tags)
           (setq merged (append curr tags))))
    (when merged
      (f-write (concat (s-join "\n" (xseq-sneat merged)) "\n") 'utf-8
               (f-join xtag--files fname))
      (xseq-sneat delta))))

(defun xtag-multi (prompt &optional closure match-required)
  "if CLOSURE is t, then it returns an xprompt-crm
function that looks like:
  (xprompt-crm \"PREFIX KIND\" \"PREFIX-KIND\")

Otherwise it directly makes the above call.

It runs the prompt through xtag-file, so if the
(modified) string exists in xtag--file-lookup,
then your prompt will match a tag file name.
Build your prompt in a way that will translate
to a file name (via xtag-file), or insert a new
key into xtag--file-lookup.

In other words, it prompts the user with the
PREFIX KIND message; and saves tags to the
PREFIX-KIND tag file (minus the tailing s)."
  (if closure
      (lambda () (xprompt-crm prompt (xtag-file prompt) match-required))
    (xprompt-crm prompt (xtag-file prompt))))

(defun xtag-single (prompt &optional closure match-required)
  "The same as xtag-multi, except it returns an
xtag-file prompt."
  (if closure
      (lambda () (xtag-pick (xtag-file prompt) prompt :match match-required))
    (xtag-pick (xtag-file prompt) prompt :match match-required)))

;; TODO: use match pattern
(setq xtag--file-lookup (ht ("tv-actor"          "tv-actor")
                            ("tv-actors"         "tv-actor")
                            ("tv-genre"          "tv-genre")
                            ("tv-genres"         "tv-genre")
                            ("tv-writer"         "tv-writer")
                            ("tv-writers"        "tv-writer")
                            ("tv-director"       "tv-director")
                            ("tv-directors"      "tv-director")
                            ("tv-context"        "tv-context")
                            ("tv-contexts"       "tv-context")
                            ("tv-category"       "tv-category")
                            ("tv-categories"     "tv-category")
                            ("article-category"  "article-category")
                            ("article"           "article-category")
                            ("music-artist"      "music-artist")
                            ("music-artists"     "music-artist")
                            ("music-genre"       "music-genre")
                            ("music-genres"      "music-genre")
                            ("game-genre"        "game-genre")
                            ("game-genres"       "game-genre")
                            ("game-publisher"    "game-publisher")
                            ("game-publishers"   "game-publisher")
                            ("game-developer"    "game-developer")
                            ("game-developers"   "game-developer")
                            ("played-state"      "state")
                            ("game-state"        "release-state")
                            ("release-state"     "release-state")
                            ("music-context"     "music-context")
                            ("music-contexts"    "music-context")
                            ("person"            "person-type")
                            ("person-type"       "person-type")
                            ("type-of-person"    "person-type")
                            ("quote"             "quote-type")
                            ("quote-type"        "quote-type")
                            ("type-of-quote"     "quote-type")
                            ("idea"              "idea-type")
                            ("idea-type"         "idea-type")
                            ("type-of-idea"      "idea-type")
                            ("project"           "project-type")
                            ("project-type"      "project-type")
                            ("type-of-project"   "project-type")
                            ("state"             "state")
                            ("watched-state"     "state")))

(defun xtag-file (&rest key)
  "Lookup the correct tag file name by its KEY.

KEY can be multiple values, which are joined.

This function is very flexible, and you can insert
spaces, capitals etc, and it will do its best to match
it to a tag file:
  e.g. \"   TV \"  \"   categories  \"  => \"tv-categories\"

The subsequent lookup will return the file name:
  tv-category
This value is usable in xtag-pick, or any function that
does tag lookups."
  (gethash (replace-regexp-in-string
            " +" "-"
            (downcase (s-trim (s-join " " key))))
           xtag--file-lookup))
