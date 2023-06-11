(use-package! org-ref
  :init (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
	      org-ref-insert-cite-function 'org-ref-cite-insert-helm
	      org-ref-insert-label-function 'org-ref-insert-label-link
	      org-ref-insert-ref-function 'org-ref-insert-ref-link
	      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))
  :bind (("M-]" . org-ref-insert-link-hydra/body)
         ("M-[" . org-ref-cite-insert-helm)))

(require 'org-ref) ; docs says it's needed
(require 'org-ref-helm) ; this too
