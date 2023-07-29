(defun ximg-insert-org ()
  "Insert an Org-Mode image link at point."
  (interactive)
  (insert (ximg-block-create)))

(defvar ximg--smenu-sources nil "The menu choices for 'smenu--img-source. ")
(setq ximg--smenu-sources
      '((?d "download" "download")
        (?l "local" "local")
        (?e "embed" "embed")))

(cl-defun ximg-block-create (&key tag name desc)
  "Return an image block like so: [[<LINK>][DESC]],
but it will prompt to download the image; pick it
from local files; or embed a URL.

TAG is a prefix for the file name to help keep the
file system tidy: e.g. foo--imgname.jpg. If you do
not provide one, and you don't set one via the the
prompted file picker, you will be asked to pick one
from the \"image\" tags file.

NAME is the name of the file. It's trimmed; spaces
replaced with _; lowercased. You do not need to
provide an extension -- the URL should end in an
extension, and it will use that. You do not need
to provide a name because you are prompted to pick
a file location via completing-read. This is just
for convenience, because it will filter matching
names so you can make a better decision. Generally
this is an option for capture templates to pass the
title into.

DESC is the link description. If you don't provide
one, it will prompt you. You can use the default
by hitting return."
  (interactive)
  (let* ((choice (nth 0 (smenu "Make link from" ximg--smenu-sources)))
         (path
          (cond
           ((string= choice "local")
            (f-pick (directory-files my/imgs t (image-file-name-regexp))
                    :root my/imgs
                    :initial-input (ximg--search-string tag name)
                    :require-match t))
           ((string= choice "download")
            (let* ((url (xurl-simg (xprompt "Image URL")))
                   (fpath (f-pick (directory-files my/imgs t (image-file-name-regexp))
                                  :root my/imgs
                                  :prompt "Overwrite image?"
                                  :initial-input (ximg--search-string tag name)
                                  :confirm t)))
              ;; now properly format the name
              (setq fpath
                    (f-short
                     (f-join
                      (f-dirname fpath) (ximg--fname (f-base+ fpath) tag (f-ext url)))))
              (print fpath)
              (xcheck (cons "File path is not a valid image path" fpath)
                      (ximg--dir-p fpath)
                      (ximg--fname-p (f-base+ fpath)))
              (ximg--fetch url fpath :overwrite t)))
           ((string= choice "embed")
            (xurl-simg (xprompt "Image URL"))))))

    ;; if user provides empty link description, it defaults to Img
    (let* ((d (or desc (xprompt "Link description [Img]"))))
      (ximg-block path (if (xstr-t d) d "Img")))))

(defun ximg--dir-p (path)
  "Determine if PATH is my/imgs."
  (not (eq nil (string-match (concat "^" my/imgs "\\(\/\\|$\\)") path))))

(defun ximg--fname-p (fname)
  "Determine if FNAME is a valid image file name."
  (and
   (ximg--tag-p fname)
   (string-match "^[-a-z_\\.]+$" fname)
   (org-file-image-p fname)))

(defun ximg--set-tag (name tag)
  "Apply tag-- to name if it doesn't have one, otherwise
ignore it (because it has one)."
  (if (string-match "^[a-zA-Z0-9_]+--" name)
      name
    (if (eq tag nil)
        (concat (xtag-pick 'image "Pick an image tag") "--" name)
      (concat tag "--" name))))

(defun ximg--set-ext (name ext)
  "Set extension if it doesn't have one; replace extension in
name that doesn't match ext."
  (if (org-file-image-p name)
      (replace-regexp-in-string (image-file-name-regexp) (concat "." ext) name)
    (concat name "." ext)))

(defun ximg--tag-p (s)
  "Test that S matches ^tag--."
  (if (eq s nil) nil (string-match "^[_0-9a-zA-Z]+--" s)))

(defun ximg--fname (name tag ext)
  "Add a tag, extension, slugify, trim, and lowercase the name."
  (let* ((n (my/slugify name "-"))
         (with-tag (ximg--set-tag n tag))
         (with-ext (ximg--set-ext with-tag ext)))
    (downcase with-ext)))

(defun ximg--search-string (tag name)
  "Produce a string like 'tag-- foo bar' for easy
completing-read filtering. Either arg can be nil.
If both args are nil it returns a blank string."
  (let* ((tidy )
         (result ""))
    (unless (eq tag nil) (setq result (concat result tag "-- ")))
    (unless (eq name nil) (setq result (concat result name)))
    (unless (string-blank-p result)
      (setq result (replace-regexp-in-string
                    " \\{2,\\}" " " (downcase (s-trim result)))))
    result))

(cl-defun ximg--fetch (url output &key overwrite)
  "Fetch an image (URL) and store it in OUTPUT; return the path."
  (when (url-copy-file url output overwrite)
    (shell-command (format "mogrify -strip '%s'" output))
    output))

(defun ximg--special-path (path)
  "Change ~/org/img to ../img."
  (unless (boundp 'my/imgs) (error "You must set my/imgs")())
  (replace-regexp-in-string (concat "^" my/imgs) "../img" path))

(defun ximg-block (path &optional desc)
  "Return an image block string initialised with the
PATH. DESC is the link description.

Returns a string: [[https://foo.com/foo.jpg][Img]]"
  (org-link-make-string
   (ximg--special-path path)
   (or desc "Img")))
