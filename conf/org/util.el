(setq my/org-agenda-dir (concat org-directory "/agenda/"))

(defun my/ls-agenda-dir ()
  "Get a directory listing of agenda files inside the agenda directory."
  (directory-files my/org-agenda-dir nil org-agenda-file-regexp)) ; NEVER return full path

(defun my/fix-org-file-name (name)
  "Ensure that there is an org extension, and replace spaces with underscores."
  (let* ((tidy (replace-regexp-in-string "\\.org$" ""
                                         (replace-regexp-in-string " " "_" name))))
    (concat tidy ".org")))

(defun my/abs-agenda-fpath (fname)
  "Given an agenda file name, return the full, absolute path."
  (concat my/org-agenda-dir fname))

(defun my/pick-agenda-file ()
  "Pick an agenda file from a completion list, otherwise create it."
  ; DON'T create files in the agenda directory WITH SPACES or WITHOUT
  ; the .ORG EXTENSION. To be safe: use this picker when creating files too.
  (my/abs-agenda-fpath
   (my/fix-org-file-name
    (completing-read "Choose an agenda file: " (my/ls-agenda-dir)) )))

(defun my/find-agenda-file ()
  "Find an existing agenda file, or create one."
  (interactive)
  (mkdir my/org-agenda-dir t)
  (set-buffer
   (org-capture-target-buffer
    (my/pick-agenda-file)))
  (goto-char (point-max)))

(defun my/read-file (file)
  "Read the contents of a file."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(setq my/templates-dir (concat doom-user-dir "/templates"))

(defun my/template-path (name)
  "Return the full template path for the given file name (without extension)."
  (concat my/templates-dir "/" name ".org"))

(defun my/template (name)
  "Return the template contents for the given extensionless file name."
  (my/read-file (my/template-path name)))

(defun my/agenda-template ()
  "Return the agenda template contents. Use this for capture templates."
  (my/template "agenda"))
