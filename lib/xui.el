;; -*- lexical-binding: t; -*-

(defun my/doom-popup-buffer (fpath &optional pos valid-p)
  "Open a buffer in a Doom popup window.
FPATH: The file path to open.
\nPOS: A function that deteermins the cursor pos; '(point-max)' by default.
\nVALID-P: A predicate that executes upon save and close.
It receives a buffer (BUF). Return t to successfully save and close."
  (interactive "FFile to edit: ")
  (let ((buf (find-file-noselect fpath)))
    (+popup-buffer buf '((size . 0.4) (side . bottom) (quit . t) (select . t)))
    (select-window (get-buffer-window buf))  ; Sometimes it doesn't focus.
    (with-current-buffer buf
      (goto-char (if pos (funcall foo) (point-max)))

      ;; Save and close.
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (if valid-p
                           ;; Execute valid-p to successfully close,
                           ;; e.g., validate the buffer.
                           (if (funcall valid-p buf)
                               (progn (save-buffer)
                                      (kill-buffer))
                             (message "Invalid buffer; fix before saving."))
                         ;; Close normally
                         (save-buffer)
                         (kill-buffer))))

      ;; Cancel and close.
      (local-set-key (kbd "C-c C-k")
                     (lambda ()
                       (interactive)
                       (set-buffer-modified-p nil)
                       (kill-buffer))))))
