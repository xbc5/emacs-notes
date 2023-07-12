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

Returns the modified list of tags saved to file."
  (mkdir xtag--files t)
  (let* ((curr (xtag-get fname))
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
