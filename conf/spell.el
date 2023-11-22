;; frontends
;;   - flyspell: handles highlighting;
;;   - ispell: (builtin) handles interactive spell-checking;
;; backends
;;   - aspell: slow, but focuses on quality of suggestions
;;
;; both flyspell and ispell can use hunspell, aspell, ispell as backends.

(after! flyspell
  (setq flyspell-issue-message-flag nil) ; emits a message for each word, causes huge slowdown
  (add-hook 'text-mode 'flyspell-mode) ; normal spell check
  (add-hook 'prog-mode  flyspell-prog-mode)) ; for source code

; builtin
(setq ispell-dictionary "british"
      ispell-personal-dictionary "~/.aspell.en.pws" ; nil means use the backend checker's default
      ispell-program-name "aspell") ; set backend
