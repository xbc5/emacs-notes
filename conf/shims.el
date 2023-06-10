(defun dnd-unescape-uri () nil) ;; BUG: something something undefined

; org-ql for some reason triggers this function, but
; the author says that their package doesn't use it --
; it seems to be a bug with Emacs:
; https://github.com/alphapapa/org-ql/issues/345
(defalias 'byte-run--set-speed
  #'(lambda (f _args val)
      (list 'function-put (list 'quote f)
        ''speed (list 'quote val))))

; BUG(#19): remove this shim when fixed
(defun my/unfuck-agenda-files ()
  "org-ref clobbers org-agenda-files somehow, use this to unfuck that for fuck sake."
  (interactive)
  (setq org-agenda-files '("~/org/agenda")))
