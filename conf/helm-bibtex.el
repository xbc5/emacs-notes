(use-package! helm-bibtex
  :after (org-roam)
  :bind ("M-p" . helm-bibtex)
  :config
  (setq bibtex-completion-display-formats
        '((t . "${=has-pdf=:1}${=has-note=:1} ${author:36} ${title:*} ${year:4}"))
        bibtex-completion-additional-search-fields '(keywords tags)
        bibtex-completion-bibliography my/bib-files ; TODO: rescan when a new bib file is added
        bibtex-completion-notes-path my/lit-notes)
  (require 'org-ref))
