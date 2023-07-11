(package! rg)
(package! free-keys)
(package! yaml)
(package! el-mock)

(when (package! org)
  (package! org-fancy-priorities)
  (package! org-ql)
  (package! plantuml-mode))

;; when re-pinning to module, remember to stop using the
;; org-roam-bibtex recipe, use the package from MELPA intead.
(unpin! org-roam)

(when (package! org-roam)
  (package! vulpea)
  (package! org-roam-server :disable t) ; not yet supported for roam v2
  (when (package! org-ref) ; we don't want ref is no roam
    (when (package! helm-bibtex) ; we don't need if no ref
      (package! bibtex-completion)
      ;; we must use a recipe if we unpin org-roam
      (package! org-roam-bibtex
        :recipe (:host github :repo "org-roam/org-roam-bibtex"))))

  ;; these deps are a mess, I am not going to even try
  (package! org-roam-dblocks ; deps on: org-tags-filter; plisty
    :recipe (:host github
             :repo "chrisbarrett/nursery"
             :files ("lisp/org-roam-dblocks.el")))
  (package! org-tags-filter ; dep for: dblocks
    :recipe (:host github
             :repo "chrisbarrett/nursery"
             :files ("lisp/org-tags-filter.el")))
  (package! plisty ; dep for: dblocks
    :recipe (:host github
             :repo "chrisbarrett/nursery"
             :files ("lisp/plisty.el")))
  ;; deps on: dash org-drill org-roam-dailies org-roam-node org-tags-filter plisty ts
  ;; dep for: org-roam-search
  (package! org-roam-review
    :recipe (:host github
             :repo "chrisbarrett/nursery"
             :files ("lisp/org-roam-review.el"))))
