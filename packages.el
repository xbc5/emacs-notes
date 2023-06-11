(package! ox-pandoc) ; export to multiple formats
(package! rg)

; org
(package! org-fancy-priorities)
(package! org-roam-server :disable t) ; not yet supported for roam v2
(package! org-ql)

; orb
(package! org-roam-bibtex)
(package! bibtex-completion) ; required by helm|ivy-bibtex
(package! org-ref)
(package! helm-bibtex)

(package! plantuml-mode)

; nursery: i.e. misc tools from https://github.com/chrisbarrett/nursery
; the config for these is conf/nursery.
;
; dblocks: canned searches in dynamic org blocks
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

; search: display search results in a temp buffer with a preview
; deps on: org-roam-review; async; dash; magit[-diff]; org-roam[-review]; pcre2el;
;(package! org-roam-search 
;          :recipe (:host github
;                   :repo "chrisbarrett/nursery"
;                   :files ("lisp/org-roam-search.el")))
; 
; review: a system to review evergreen notes
; deps on: dash org-drill org-roam-dailies org-roam-node org-tags-filter plisty ts
; dep for: org-roam-search
(package! org-roam-review
          :recipe (:host github
                   :repo "chrisbarrett/nursery"
                   :files ("lisp/org-roam-review.el")))

(package! free-keys)
