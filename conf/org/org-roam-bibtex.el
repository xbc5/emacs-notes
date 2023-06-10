(use-package! org-roam-bibtex
  :config
  (setq orb-edit-note t  ; enable BibTeX key expansion in capture templates
        orb-preformat-keywords '("citekey" "date" "pdf?" "author" "title"))) ; for expansion
