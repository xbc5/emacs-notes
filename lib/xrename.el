;; onsave
;;   1. save file
;;   2. rename file
;;   3. reload file (edit)
;;   4. delete old buffer

(defun xrename--save (new-name)
  (when (buffer-file-name)
    (rename-file (f-filename (buffer-file-name)) new-name))
  (write-file new-name)
  new-name)
