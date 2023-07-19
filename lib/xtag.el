;; -*- lexical-binding: t; -*-
(require 'ht)

(cl-defun xtag-pick (key prompt &optional require-match)
  "Pick a tag from the tag file that matches KEY --
create one if it doesn't exist.

PROMPT is the display message.
REQUIRE-MATCH when t will require the user to pick
an existing value, and cannot create one."
  (let* ((tags (xtag-get key))
         (choice
          (replace-regexp-in-string
           " +" "_" (s-trim
                     (completing-read (concat prompt ": ") tags nil require-match)))))
    (unless (member choice tags) (xtag-write key choice))
    choice))

(defun xtag-tagify (tag)
  ;; xstr-neat removes excess spaces, so do lazy replace
  (replace-regexp-in-string " +?" "_" (xstr-neat tag)))

(defun xtag-p (key tag)
  "Return t if TAG exists inside tags file (KEY)."
  (not (eq nil (member tag (xtag-get key)))))

(defun xtag-get (key)
  "Return the tags from the given tag file KEY."
  (let ((fpath (xtag-file key)))
    (split-string
     (my/read-file fpath)
     "\n" t " ")))

(defun xtag-write (key tags)
  "Write TAGS to tag file identified by KEY.
TAGS can be a string, list, or nil. It will trim, sort,
and remove duplicates -- so there's no need to perform
this yourself; it will ignore nil.

Returns the list of tags saved to file."
  (let* ((curr (xtag-get key))
         (merged nil)
         (delta nil))
    (cond ((stringp tags)
           (setq delta (list tags))
           (setq merged (append curr delta)))
          ((seqp tags)
           (setq delta tags)
           (setq merged (append curr tags)))
          (t (error "Cannot write %s (%s) to tag file" tags (type-of tags))))
    (when merged
      (f-write (concat (s-join "\n" (xseq-sneat merged)) "\n") 'utf-8 (xtag-file key))
      (xseq-sneat delta))))

(defun xtag--crm (key prompt &optional require-match)
  (xtag-write key
              (completing-read-multiple
               (concat prompt ": ") (xtag-get key) nil require-match)))

(defun xtag-multi (key prompt &optional closure require-match)
  "Perform a completing-read to pick multiple tags
from tag file that corresponds to KEY.

PROMPT is the display message.

if CLOSURE is t, then it returns an xtag--crm
lambda that looks like:
  (lambda () (xtag--crm 'key \"prompt\"))

If REQUIRE-MATCH, the user must select at least one tag,
and cannot create any. "
  (if closure
      (lambda () (xtag--crm key prompt require-match))
    (xtag--crm key prompt require-match)))

(defun xtag-single (key prompt &optional closure require-match)
  "The same as xtag-multi, except it returns an
xtag-file prompt."
  (if closure
      (lambda () (xtag-pick key prompt require-match))
    (xtag-pick key prompt require-match)))

(setq xtag--file-lookup (ht ('article-category   "article-category")
                            ('game-developer     "game-developer")
                            ('game-genre         "game-genre")
                            ('game-played-state  "state")
                            ('game-publisher     "game-publisher")
                            ('idea-type          "idea-type")
                            ('image              "image")
                            ('music-artist       "music-artist")
                            ('music-context      "music-context")
                            ('music-genre        "music-genre")
                            ('needs-cover        "needs-cover")
                            ('needs-rating       "needs-rating")
                            ('needs-year         "needs-year")
                            ('needs-state        "needs-state")
                            ('needs-url          "needs-url")
                            ('note-category      "note-category")
                            ('person-type        "person-type")
                            ('project-type       "project-type")
                            ('quote-type         "quote-type")
                            ('release-state      "release-state")
                            ('state              "state")
                            ('tv-actor           "tv-actor")
                            ('tv-category        "tv-category")
                            ('tv-context         "tv-context")
                            ('tv-director        "tv-director")
                            ('tv-genre           "tv-genre")
                            ('tv-writer          "tv-writer")
                            ('watched-state      "state")))

(defun xtag-file (key)
  "Lookup the correct tag file name by its KEY.

KEY is a symbol.

Returns a string."
  (when (xnil-ish 'xtag-files) (error "You must set xtag-files."))
  (mkdir xtag-files t)
  (let* ((path (f-join xtag-files
                       (or (ht-get xtag--file-lookup key)
                           (error "No tag file for '%s" key)))))
    (unless (f-exists-p path) (f-touch path))
    path))
