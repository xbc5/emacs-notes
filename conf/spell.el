;; frontends
;;   - flyspell|spell-fu: handles highlighting;
;;   - ispell: (builtin) handles interactive spell-checking;
;; backends
;;   - aspell: slow, but focuses on quality of suggestions
;;
;; both flyspell and ispell can use hunspell, aspell, ispell as backends.
;;
;; WARN: spell-fu will highlight all words as incorrect on occasion possibly afte you change
;; the dictionary or codepage -- you must remove the cache: ~/.emacs.d/.local/etc/spell-fu/*

; disabled in Doom modules, but I've left it here in-case it's re-enabled.
; It causes multiple issues: sometimes performance; it interferes with Org captures.
; Emacs uses spell-fu by default (if you don't enable flyspell).
(after! flyspell
  (setq flyspell-issue-message-flag nil) ; emits a message for each word, causes huge slowdown
  (add-hook 'text-mode 'flyspell-mode) ; normal spell check
  (add-hook 'prog-mode 'flyspell-prog-mode)) ; for source code

; Set Doom module: (spell +aspell); it's enabled by default (without specification).
;  the encoding is provided by aspell under /usr/lib64/aspell-*/*.cset, and the iso-8859-1 cset
; file is provided by Fedora (the aspell-en pkg). There is a lack of UTF-8, but it seems to work
;  regardless.
; The two packages that you need if you use aspell: aspell aspell-en.
; [1] As far as I can tell ispell-dictionary is a key to lookup aspell dictionary specs
;     from ispell-dictionary-base-alist. These dicts live under /usr/lib64/aspell-*.
(after! ispell
  (setq ispell-dictionary "british" ; a key for dict specs[1]; also sets ispell-local-dictionary
        ispell-local-dictionary-alist
          '(("british"
             "[[:alpha:]]"
             "[^[:alpha:]]"
             "[']"
             t
             ("-d" "en_GB")
             nil
             utf-8)) ; allow 'quotes': "[']" => "['`]" allows backticks for example
        ispell-personal-dictionary "~/.aspell.en.pws"))
