(defun xname-new (dir name &optional full-path)
  "Get a suitable name for a file that does not
clash with existing files in DIR.

NAME is the preferred name, that this function
will mutate to make it unique -- including replacing
spaces with underscores; removing special characters;
trimming; downcasing; iterating (appending a number);
and truncating -- all while preserving the extension,
and respecting xname-max-len.

FULL-PATH, when non-nil, will prefix DIR to the
returned name."
  (let* ((new (xname--find dir (xname--fix name) #'f-exists-p)))
    (if full-path (f-join dir new) new)))

(defun xname-set-visited (new-fname)
  "Change the visited file name of the current buffer
to a value close to NEW-FNAME. If NEW-FNAME exists
within the same directory, it will iterate (append
a number). It will also trim the name down to
xname-max-len, while preserving the ext.

Returns the new path, or nil (if not set)."
  (xname--set-visited new-fname #'buffer-file-name #'set-visited-file-name #'f-exists-p))

(defun xname-change (&optional new)
  "Set both the visited file name for the current
buffer, and rename the file on disk, to NEW (or a
value close to it -- see xname-new for details)."
  (interactive)
  (unless (eq new nil)
    (xcheck "Cannot rename file to blank string." (not (string-blank-p (s-trim new)))))

  (let* ((curr (buffer-file-name)))
    (when curr
      (setq new (xname-set-visited
                 (if new new (xprompt "New file name" t))))
      (unless (string= curr new)
        (rename-file curr new)))))

(defun xname--fmatch (dir match)
  "Given a DIR, MATCH all files within it --
excluding hidden files and directories."
  (setq dir (f-expand dir))
  (cl-remove-if (lambda (p)
                  (string-match-p ;; hidden directories within chosen directory
                   "/\\." (replace-regexp-in-string (concat "^" dir) "" p))) ; remove base from match
                (directory-files-recursively dir match)))

(defun xname-change-all (fpaths get-name renamer file-checker &optional preserve-ext cache)
  "FPATHS is a list of file paths to affect.

GET-NAME is a function that returns the new name
when given a path for the affected file:
  (get PATH) => new name.
You do not need to format returned name.

RENAMER is a function that takes and old
and new path, and will rename (move) the
file -- typically this is (rename-file).

FILE-CHECKER is a predicate function that
checks for the existence of a file. It takes
one are -- a path: typically (f-exists-p).

PRESERVE-EXT will keep the extension from the
old file name, and append it to the new.

CACHE is a path to a file to store progress.
There are possibly thousands of files, and
restarting can cost a lot of time. This file
stores the names of files yet to be processed.
Typically you will use /tmp/foo.
"
  ;; use cache if we have it
  (setq fpaths (if (and cache (f-exists-p cache))
                   (xfs-read-lines cache t)
                 fpaths))
  (let* ((cached fpaths))
    (seq-doseq (old-path fpaths)
      (let* ((ext (if preserve-ext (f-ext+ old-path t) ""))
             (title (funcall get-name old-path))
             (old-fname (f-base+ old-path))
             (new-path (xname--get
                        (f-dirname old-path) old-fname (concat title ext) file-checker)))
        (kill-buffer (find-buffer-visiting old-path))
        (unless (string= old-path new-path)
          (funcall renamer old-path new-path))
        (when cache
          (setq cached (remove old-path cached)) ;; write cache
          (xfs-write-lines cache cached))))
    (when (and cache (length= cached 0) (file-exists-p cache)) ;; remove if cache is useless
      (delete-file cache))))


;; -- PRIVATE --


(defun xname--append-num (name n)
  "Given a NAME an [N]umber (>0), return a modified
string that includes it at the end of the name, for
example:

(xname--append-num \"foo.org\" 1) => \"foo(1).org\"
(xname--append-num \"foo\" 2)     => \"foo(2)\"

This will respect xname-max-len, and truncate
the part of the name that does not fit."
  (let* ((base (f-base name))
         (ext (or (f-ext+ name t) "")) ; prevent nil error
         (len (- (xname--max-len)
                 (length ext)
                 (+ (xnum-len n) 2)))) ; make room for (1), (2) etc
    (format "%s(%d)%s" (xstr-truncate base len) n ext)))

(defun xname--find (dir fname fn)
  "Find a name that does not conflict with an existing file.

The FNAME is the preferred name, which may or may not be the
result.

DIR is the directory which FNAME should reside.

FN is a function to test it the file exists, typically
it's f-exists-p. It takes one arg: a full path."
  (let* ((iter 0) ; becomes name(n).ext
         (rev nil) ; temp storage for file name revisions
         (final nil)) ; the name that worked
    (while (not final)
      (setq rev (if (= iter 0) fname (xname--append-num fname iter))) ; don't use number on iter 0
      (if (funcall fn (f-join dir rev)) ; if revision exists
          (setq iter (+ iter 1))
        (setq final rev)))
    final))

(defun xname--max-len ()
  (this-or-that 'xname-max-len 255))

(when (< (xname--max-len) 30)
  (error "xname-max-len must be > 30"))

(defun xname--append (fname suffix)
  "Append the SUFFIX to FNAME while respecting
the xname-max-len -- i.e. it will truncate FNAME
to make the SUFFIX fit."
  (concat
   (xstr-truncate
    fname (- (xname--max-len) (length suffix)))
   suffix))

(defun xname--fix (name)
  "Trim, slugify, downcase NAME, replace special
chars, and make it fit within max length
constraints (including the extension)."
  (let* ((ext (f-ext+ name))
         (fname (downcase (xfs-slugify (f-base name)))))
    (if ext
        (xname--append fname (concat "." ext))
      (xstr-truncate fname (xname--max-len)))))

(defun xname--get (dir old new file-checker)
  "Get a name that does not collide with an existing
name in the file system.

OLD is the name of the file to change.

NEW is the preferred name that you wish to change.
You do not need to format it, this function will
do that for you. You DO need to provide an extension
if you want one.

DIR is the directory which OLD and NEW exist.

FILE-CHECKER is a predicate function that takes
a path argument, and checks whether that file exists."
  (let* ((fixed (xname--fix new)))
    (f-join dir (if (string= old fixed) fixed
                  (xname--find dir fixed file-checker)))))

(defun xname--set-visited (new-fname bfr-fname visited-fname file-checker)
  "xname-set-visited wraps this and injects the
functions that do the heavy lifting -- i.e. use
xname-set-visited instead.

NEW-FNAME is the preferred visited file name
(not path) to set the buffer to.

BFR-FNAME is a function that gets the current
visited file name from the buffer. Typically
this is (buffer-file-name).

VISITED-FNAME is a function that sets the visited
file name of the buffer. Typically this is
(set-visited-file-name).

FILE-CHECKER is a predicate function that determined
if a file exists. Typically this is (f-exists-p)."
  (when (funcall bfr-fname)
    (let* ((old (funcall bfr-fname))
           (new  (xname--get
                  (f-dirname old)
                  (f-base+ old)
                  new-fname
                  file-checker)))
      (unless (string= old new) (funcall visited-fname new nil t))
      new)))
