;; frontends
;;   - flyspell|spell-fu: handles highlighting;
;;   - ispell: (builtin) handles interactive spell-checking;
;; backends
;;   - aspell: slow, but focuses on quality of suggestions
;;
;; both flyspell and ispell can use hunspell, aspell, ispell as backends.

; disabled in Doom modules, but I've left it here in-case it's re-enabled.
; It causes multiple issues: sometimes performance; it interferes with Org captures.
; Emacs uses spell-fu by default (if you don't enable flyspell).
(after! flyspell
  (setq flyspell-issue-message-flag nil) ; emits a message for each word, causes huge slowdown
  (add-hook 'text-mode 'flyspell-mode) ; normal spell check
  (add-hook 'prog-mode 'flyspell-prog-mode)) ; for source code

; Set Doom module: (spell +aspell); it's enabled by default (without specification).
; the encoding is provided by aspell under /usr/lib64/aspell-*/*.cset, and the iso8859-1 cset
; file is provided by Fedora (the aspell-en pkg). There is a lack of UTF-8, but this is 7-bit
; ASCII. The two packages that you need if you use aspell: aspell aspell-en.
(after! ispell
  (setq ispell-dictionary "en_GB" ; aspell dicts in /usr/lib64/aspell-*
        ispell-local-dictionary "en_GB"
        ispell-personal-dictionary "~/.aspell.en.pws" ; nil means use the backend checker's default
        ispell-local-dictionary-alist 
          '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil iso8859-1))))
