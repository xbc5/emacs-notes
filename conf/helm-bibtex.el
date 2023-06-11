(use-package! helm-bibtex
  :bind ("M-n" . helm-bibtex)
  :config
  (setq bibtex-completion-display-formats
        '((t . "${=has-pdf=:1}${=has-note=:1} ${author:36} ${title:*} ${year:4}"))
        bibtex-completion-additional-search-fields '(keywords tags)
        bibtex-completion-bibliography my/refs
        bibtex-completion-notes-path my/bib-notes))
