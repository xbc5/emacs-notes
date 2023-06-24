(map! "M-]" #'org-ref-insert-link-hydra/body
      "M-[" #'org-ref-cite-insert-helm
      "M-p" #'helm-bibtex)

(setq bibtex-completion-bibliography my/bib-files
      bibtex-completion-notes-path my/lit-notes)

(after! org-roam
  (setq ;; org-ref
   org-ref-insert-link-function 'org-ref-insert-link-hydra/body
   org-ref-insert-cite-function 'org-ref-cite-insert-helm
   org-ref-insert-label-function 'org-ref-insert-label-link
   org-ref-insert-ref-function 'org-ref-insert-ref-link
   org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))

   ;; bibtex
   bibtex-completion-display-formats
   '((t . "${=has-pdf=:1}${=has-note=:1} ${author:36} ${title:*} ${year:4}"))
   bibtex-completion-additional-search-fields '(keywords tags)

   ;; orb
   orb-edit-note t  ; enable BibTeX key expansion in capture templates
   orb-preformat-keywords '("citekey" "date" "pdf?" "author" "title"))

  (add-hook 'org-roam-mode-hook #'org-roam-bibtex-mode))


;; WARN: slows down boot by ~5s
;; It's required so that orb will index lit notes
;; in order to open them via helm. I have tried
;; everything from after! org-roam, to a roam hook
;; but loading this here is necessary.
(require 'org-ref)
(require 'org-ref-helm)
