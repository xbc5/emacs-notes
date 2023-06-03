(map! :desc "Insert a citation"
      :map org-mode-map "C-c ]"
      #'org-ref-insert-link-hydra/body)

(require 'org-ref-helm)
(use-package! org-ref-helm
  :init (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
	      org-ref-insert-cite-function 'org-ref-cite-insert-ivy
	      org-ref-insert-label-function 'org-ref-insert-label-link
	      org-ref-insert-ref-function 'org-ref-insert-ref-link
	      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))))
(let ((notes-dir "~/org/bib/notes") (bib-file "~/org/bib/refs.bib"))
  (use-package! org-ref
                :config
                (setq org-ref-notes-directory notes-dir
                      org-ref-default-bibliography (list bib-file)))
  (setq bibtex-completion-bibliography bib-file
        bibtex-completion-notes-path notes-dir))
(after! org-roam-bibtex
        (require 'org-ref)
        (org-roam-bibtex-mode))

; fuzzy select a cite key, parsed from bibtex files
(defun my/get-bibtex-key (node)
  (completing-read "Citation key: "
                   (mapcar #'(lambda (x) (cdr (assoc "=key=" x)))
                           (bibtex-completion-candidates))))
