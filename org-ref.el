(defun dnd-unescape-uri () nil) ;; BUG: something something undefined

(require 'org-ref-helm)
(setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
      org-ref-insert-cite-function 'org-ref-cite-insert-helm
      org-ref-insert-label-function 'org-ref-insert-label-link
      org-ref-insert-ref-function 'org-ref-insert-ref-link
      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))

(setq my/bib-file "~/org/bib/refs.bib"
      my/bib-pdfs "~/org/bib/pdfs"
      my/bib-notes-dir "~/org/bib/notes"
      my/bib-notes-file "~/org/bib/notes.org") ; FIXME: I am not entirely sure what this does

;; built-in packages
(setq reftex-default-bibliography my/bib-file
      bibtex-completion-bibliography my/bib-file
      bibtex-completion-library-path my/bib-pdfs
      bibtex-completion-notes-path my/bib-notes-dir
      bibtex-completion-pdf-open-function (lambda (fpath)
                                            (start-process "open" "*open*" "open" fpath)))
(use-package! org-ref
              :config
              (setq org-ref-notes-directory my/bib-notes-dir
                    org-ref-bibliography-notes my/bib-notes-file
                    org-ref-default-bibliography (list my/bib-file)
                    org-ref-pdf-directory my/bib-pdfs))

(map! :desc "Insert a citation"
      :map org-mode-map "C-c ]"
      #'org-ref-insert-link-hydra/body)

