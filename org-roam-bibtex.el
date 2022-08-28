; TODO: bind helm-bibtex
; FIXME: this template is broken.
(after! org-roam-bibtex
  (after! org-roam
    (setq orb-templates
          '(("b" "bib" plain (function org-roam-capture--get-point)
             "\n\n* Context\n* Notes\n* Thoughts\n* Questions"
             :file-name "bib/notes/${citekey}"
             :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS: \"bib notes\" unfinished TODO %?"
             :unnarrowed t)))
    ;(add-hook 'org-roam-mode 'org-roam-bibtex-mode)
    (require 'org-ref)
    (org-roam-bibtex-mode)))
