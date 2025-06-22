;; -*- lexical-binding: t; -*-

(defun my/doom-popup-buffer (fpath &optional pos)
  "Open a buffer in a Doom popup window.
FPATH: The file path to open.
\nPOS: The cursor position; '(point-max)' by default."
  (interactive "FFile to edit: ")
  (let ((buf (find-file-noselect fpath)))
    (+popup-buffer buf '((size . 0.4) (side . bottom) (quit . t) (select . t)))
    (select-window (get-buffer-window buf))  ; Sometimes it doesn't focus.
    (with-current-buffer buf
      (goto-char (if pos pos (point-max)))

      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (save-buffer)
                       (kill-buffer)))
      (local-set-key (kbd "C-c C-k")
                     (lambda ()
                       (interactive)
                       (set-buffer-modified-p nil)
                       (kill-buffer))))))
