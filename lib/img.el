(defvar my/smenu-image-choices nil
  "The menu choices for 'my/smenu--choose-image.
The functions should return a path: a URL or
a local file path.")
(setq my/smenu-image-choices
      '((?d "download" (lambda (tag name &optional msg)
                         (my/img--fetch-prompt tag name msg))) ; local fpath
        (?l "local" (lambda (tag name &optional msg)
                      (my/img--pick-local msg (my/img--prep-search-string name tag)))))) ; local fpath

(defun my/img-block-prompt (tag name &optional msg width)
  "Return an image block. This is a public function
and you probably want to use this.

TAG is a prefix for the file name to help keep the
file system tidy: e.g. foo--imgname.jpg.

NAME is the name of the file. It's trimmed; spaces
replaced with _; lowercased. You do not need to
provide an extension -- the URL should end in an
extension, and it will use that.

MSG is the message that you will see when prompting
you for a URL. Don't add colons or spaces -- use
'Foo URL' for example.

WIDTH is the width inserted into the ORG_ATTR prop.
It defaults to my/img-width, or 800 if that isn't set."
  (my/img--block-from-path
   (my/smenu--choose-image tag name msg) width))

(defun my/img--valid-ext-p (path)
  "Check if path has an image file extension."
  (s-matches? (image-file-name-regexp) path))

(defun my/img--fetch-prompt (tag name &optional msg)
  "Works exactly like my/img-fetch, except it prompts
for a URL. MSG is the prompt message (without a colon
or space)."
  (let* ((url (my/prompt (or msg "URL"))))
    (when (string-blank-p url) (error "You MUST provide a URL"))
    (my/img--fetch url tag name)))

(defun my/img--set-tag (name tag)
  "Apply tag-- to name if it doesn't have one, otherwise
ignore it (because it has one)."
  (if (string-match "^[a-zA-Z0-9_]+--" name)
      name
    (concat tag "--" (downcase (my/slugify name "-")))))

(defun my/img--set-ext (name ext)
  "Set extension if it doesn't have one; replace extension in
name that doesn't match ext."
  (if (my/img--valid-ext-p name)
      (replace-regexp-in-string (image-file-name-regexp) (concat "." ext) name)
    (concat name "." ext)))

(defun my/img--prep-fname (name tag ext)
  "Add a tag, extension, slugify, trim, and lowercase the name."
  (when (string-blank-p name) (error "You cannot use a blank name."))
  (let* ((n (my/slugify name "-"))
         (with-tag (my/img--set-tag n tag))
         (with-ext (my/img--set-ext with-tag ext)))
    (downcase with-ext)))

(defun my/img--prep-search-string (name &optional tag)
  ""
  (let* ((tidy (replace-regexp-in-string " \\{2,\\}" " " (downcase (s-trim name)))))
    (if (eq tag nil)
        tidy
      (format "%s-- %s" tag tidy))))

(defun my/img--prep-fpath (name tag ext)
  "Apply my/img--prep-fname to NAME, and return an absolute path
to the image."
  (concat my/imgs "/" (my/img--prep-fname name tag ext)))

(defun my/img--pick-local (&optional msg initial-input)
  "Pick an image from my/imgs.
MSG is the display message; Use INITIAL-INPUT
to initially narrow the search and make it easy
to find the relevant file."
  (f-short
   (f-join my/imgs
           (my/ls my/imgs (or msg "Choose IMG") initial-input))))

(defun my/ls (dir &optional msg initial-input)
  "Do a completing-read ls on my/imgs."
  (completing-read
   (if msg (concat msg ": ") "Choose path: ")
   (directory-files dir) nil nil initial-input))

(defun my/img--fetch (url tag name)
  "Fetch an image and store it in my/imgs; return the path."
  (unless (my/img--valid-ext-p url)
    (error (concat "No image file extension")))

  (let* ((u (s-trim url))
         (ext (file-name-extension u))
         (fname (my/img--prep-fname name tag ext))
         (fpath (my/img--prep-fpath name tag ext))
         (overwrite nil))

    (when (file-exists-p fpath)
      (let* ((choice (my/prompt (format "File '%s' exists.. [o]verwrite [r]ename" fname))))
        (print choice)
        (cond ((string= choice "o") (print "overwriting") (setf overwrite t))
              ((string= choice "r") (setf fpath
                                          (my/img--prep-fpath
                                           (my/ls my/imgs "Choose a non-existent file: " (concat tag "--")) tag ext))))))

    (when (and (not overwrite) (file-exists-p fpath))
      (error "File exists: you must choose a unique name"))

    (when (url-copy-file u fpath overwrite)
      (shell-command (format "mogrify -strip '%s'" fpath))
      fpath)))

(defun my/img--special-path (path)
  "Change ~/org/img to ../img."
  (unless (boundp 'my/imgs) (error "You must set my/imgs")())
  (replace-regexp-in-string (concat "^" my/imgs) "../img" path))

(defun my/img--default-width ()
  "Return my/img-width or 800 if unbound."
  (if (boundp 'my/img-width)
      (or my/img-width 800)
    800))

(defun my/img--block-from-path (path &optional width)
  "Return an image block string initialised with the path."
  (unless (my/file-exists-p path) (error (format "IMG does not exist: '%s'" path)))

  (let* ((p (my/img--special-path path))
         (w (or width (my/img--default-width))))
    (format "#+ATTR_ORG: :width %d\n[[%s][Img]]\n" w p)))

(defun my/img--block-from-url (url tag name)
  "Return an image block string with a stored image from URL."
  (my/img--block-from-path (my/img--fetch url tag name)))

(defun my/smenu--choose-image (tag name &optional msg)
  "Ask the user how to obtain an image via an smenu
prompt. The available option are set via my/smenu-image-choices.

It executes the chosen function and should return a path:
a URL or local file path, something suitable for an image block."
  (let* ((choice (smenu-dispatch my/smenu-image-choices)))
    (if choice (funcall (nth 2 choice) tag name msg) "")))
