(cl-defun f-pick (paths &key root prompt initial-input require-match confirm sep)
  "Provide a list of FULL PATHS, and run completing-read
on it to pick one.

PATHS a list of file paths.

INITIAL-PROMPT: for completing-read. Use this to
initially filter paths.

REQUIRE-MATCH: for completing-read. Use this if
a user MUST pick an existing file.

CONFIRM: show a PROMPT when the path exists.

PROMPT: the prompt to display when the path
exists, display this message followed by a y/n
choice.

ROOT: when set, and you pick a non-existent
path, this value is used as the directory path.
For example, you pick /foo/bar.jpg and it doesn't
exist, it will set <ROOT>/bar.jpg. This allows
you to fuzzily specify a file name without worrying
about setting a full path, because one is provided
for you:  foo.jpg => <ROOT>/foo.jpg

SEP: when you choose a file that doesn't exist,
the name may have spaces, or uppercase in it.
This function downcases filenames, and replaces
spaces with SEP. It defaults to '-'."
  (let ((do t)
        (fpath ""))
    (while do
      (setq fpath (completing-read "Pick: " paths nil require-match initial-input))
      (cond ((and (f-exists-p fpath)
                  confirm
                  (smenu-confirm (or prompt "Use existing path?")))
             ;; ask to use existing
             (setq do nil))
            ((and (f-exists-p fpath) require-match)
             ;; MUST use existing
             (setq do nil))
            (t ; can create fname
             (if root
                 (setq fpath (f-join root (my/slugify (f-base+ fpath) (or sep "-"))))
               fpath)
             (setq do nil))))
    (f-short fpath)))

(defun xtouch-new (path)
  "If a file doesn't exist at PATH, touch, and create it."
  (unless (file-regular-p path) (f-touch path)))

(defun f-fname-p (path)
  "Returns t if PATH is only a file name,
and not part of a full path."
  (and
   (eq nil (string-match-p "\/+$" path)) ; f-split ignores foo/
   (= 1 (length (f-split path)))))

(defun f-base+ (path)
  "Return the file name WITH extension."
  (let* ((ext (f-ext path))
         (is-stupid (string-match "\\.$" path))
         (base (f-base path)))
    (if (or is-stupid (xstr-t ext))
        (concat base "." ext)
      base)))

(defun f-ext+ (path &optional dot)
  "In addition to f-ext, it will downcase,
s-trim, and even include the DOT if t."
  (let* ((ext (f-ext path)))
    (cond ((and ext dot)
           (concat "." (downcase (s-trim ext))))
          (ext (downcase (s-trim ext)))
          (t ext))))

(defun my/read-file (file)
  "Read the contents of a file."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun xfs-read-lines (file &optional omit-nuls)
  "Read FILE to a list. OMIT-NULS will omit blank
lines."
  (let* ((f (s-split "\n" (my/read-file file))))
    (if omit-nuls (-filter (lambda (l) (not (string-blank-p l))) f) f)))

(defun xfs-write-lines (file lines)
  "Write LINES (list) to FILE."
  (f-write (s-join "\n" lines) 'utf-8 file))

(defun xfs-slugify (str &optional trail)
  "Take STR: trim it, replace all spaces with
underscores, and replace most specials characters
with '' -- make it suitable for a file system path."
  (replace-regexp-in-string " +" (or trail "_")
                            (s-trim (replace-regexp-in-string "[`\"'$&*^\\]" "" str))))
