(defun xorg-stringify-prop (vals)
  "Turn VALS into a single string suitable for Org properties:
- surround with \"\";
- remove excess spaces;

VALS can be a string or a list of strings. If it's a single
string, you can sparate values with ';', and it will split
on that.
  e.g. \"  foo ;  bar   x ;baz\" => '\"foo\" \"bar x\" \"baz\"'
  e.g. (\"  foo  \" \"bar  x  \" \"baz  \") => '\"foo\" \"bar x\" \"baz\"'"
  (cond ((stringp vals)
         (if (string-blank-p vals)
             ""
           (s-join " " (mapcar
                        (lambda (s) (format "\"%s\"" (xstr-neat s)))
                        (s-split ";" vals)))))
        ((eq vals nil) "")
        ((seqp vals)
         (s-join " "
                 (mapcar
                  (lambda (s) (format "\"%s\"" (xstr-neat s)))
                  vals)))
        ((numberp vals)
         (xstr vals))
        (t (error (format "Cannot stringify type: '%s'" (type-of vals)))))
  )

(defun xorg--agenda-dir-ls ()
  "Get a directory listing of agenda files inside the agenda directory."
  (directory-files xorg-agenda-dir nil org-agenda-file-regexp)) ; NEVER return full path

(defun xorg--agenda-fpath (fname)
  "Given an agenda file name, return the full, absolute path."
  (f-join xorg-agenda-dir fname))

(defun xorg-file-name-fix (name)
  "Ensure that there is an org extension, and replace spaces with underscores."
  (let* ((tidy (replace-regexp-in-string "\\.org$" ""
                                         (replace-regexp-in-string " " "_" name))))
    (concat tidy ".org")))

(defun xorg--agenda-file-pick ()
  "Pick an agenda file from a completion list, otherwise create it."
  ;; DON'T create files in the agenda directory WITH SPACES or WITHOUT
  ;; the .ORG EXTENSION. To be safe: use this picker when creating files too.
  (xorg--agenda-fpath
   (xorg-file-name-fix
    (completing-read "Choose an agenda file: " (xorg--agenda-dir-ls)))))

(defun xorg-agenda-file-find ()
  "Find an existing agenda file, or create one."
  (interactive)
  (mkdir xorg-agenda-dir t)
  (set-buffer
   (org-capture-target-buffer
    (xorg--agenda-file-pick)))
  (goto-char (point-max)))

(defun xorg-agenda-files-set ()
  "Set the files that qualify as agenda files."
  (interactive)
  (setq org-agenda-files (directory-files xorg-agenda-dir t "\\.org$")))
