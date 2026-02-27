(package! rg)
(package! yaml)

(when (package! org)
  (package! org-fancy-priorities)
  (package! org-ql)
  (package! org-drill)
  (package! plantuml-mode))

;; when re-pinning to module, remember to stop using the
;; org-roam-bibtex recipe, use the package from MELPA intead.
;; (unpin! org-roam)

;; (when (package! org-roam)
;;   (when (package! org-ref) ; we don't want ref is no roam
;;     (when (package! helm-bibtex) ; we don't need if no ref
;;       (package! bibtex-completion)
;;       ;; we must use a recipe if we unpin org-roam
;;       (package! org-roam-bibtex
;;         :recipe (:host github :repo "org-roam/org-roam-bibtex")))))
