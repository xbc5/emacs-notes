;;; ../projects/linux/emacs-notes/group/spell.el -*- lexical-binding: t; -*-

;;- see: https://www.emacswiki.org/emacs/InteractiveSpell
;; - frontends
;;   - flyspell: handles /highlighting/
;;     - uses hunspell|aspell|ispell backend
;;   - ispell: handles /interactive/ spell-checking
;;     - uses hunspell|aspell|ispell backend
;;   - spell-fu: does highlighting, and correct on selection
;;     - BUG: disabled, because words are neve un-highlighted
;; - backends
;;   - aspell: slow, but focuses on quality of suggestions
;;
(after! flyspell
  (setq flyspell-issue-message-flag nil)) ;; emits a message for each word, causes huge slowdown

(add-hook 'text-mode-hook 'flyspell-mode)       ;; for text files
(add-hook 'prog-mode-hook 'flyspell-prog-mode)  ;; for source code comments

;; ispell is built-in, 'after! ispell' appears to not work
(setq ispell-dictionary "british")
;; FIXME: use nil, but ispell for some reason falls back to ~/.emacs.d/.local/...
(setq ispell-personal-dictionary "~/.aspell.en.pws")  ;; nil means use the backend checker's default
(setq ispell-program-name "aspell")  ;; it is the backend of ispell
