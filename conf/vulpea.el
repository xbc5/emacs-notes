;; Credit to nobiot for some of these functions:
;;  https://org-roam.discourse.group/t/tips-how-to-combine-org-roam-capture-org-capture-and-other-commands-into-a-single-menu-prompt/1262
;;
;; interactive
;; (cons 1 '(2)) aka consing
;; string-match
(require 'vulpea)

(use-package! vulpea
  :after org-roam ; so that we reset keymaps
  :config
  (map! "M-n" #'my/vulpea-node-find
        "M-N" #'my/vulpea-node-find-split
        "M-I" #'my/vulpea-node-insert)

  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))

(defun my/vulpea-node-find-split ()
  "Perform a Roam node find, but open the buffer in a split."
  (interactive)
  (my/vulpea-node-find t))

(defun my/vulpea--article-body (preview-url cover-block)
  (let* ((head "* details\n%?\n* media\n"))
    (unless (or (eq cover-block nil) (string-blank-p cover-block))
      (setf head (concat head (format "%s\n\n" cover-block))))
    (unless (or (eq preview-url nil) (string-blank-p preview-url))
      (setf head (concat head (org-link-make-string preview-url "Preview"))))
    head))

(setq my/vulpea--typical-body "* meta\n* summary\n* details\n%?\n* conclusion\n")

;; TODO: add message to menu: e.g. Download source:
;; TODO: add [u]se option on file conflict
;; TODO: do URL embeds
(defun my/vulpea--capture-article (node)
  (let* ((title (org-roam-node-title node))
         (aliases (xprompt-aliases))
         (cat (my/pick-tags "article" "Article category"))
         (preview-url  (when (my/tags-p "needs-preview" cat)
                         (xprompt-url "Preview URL")))
         (cover-block  (when (my/tags-p "needs-cover" cat)
                         (ximg-block-create :tag "cover" :name title :desc "Cover IMG")))
         (rating  (when (my/tags-p "needs-rating" cat)
                    (xprompt-rating)))
         (year  (when (my/tags-p "needs-year" cat)
                  (xprompt-year)))
         (state  (when (my/tags-p "needs-state" cat)
                   (my/pick-tags "state" "Article state")))
         (roamrefs  (when (my/tags-p "needs-url" cat)
                      (xprompt-url "Homepage URL")))
         (tags (my/roam-tag-list)))
    (vulpea-create title "article/%<%Y%m%d%H%M%S>.org"
                   :properties (my/vulpea-props :type "article"
                                                :cat cat
                                                :aliases aliases
                                                :state state
                                                :year year
                                                :rating rating
                                                :roamrefs roamrefs)
                   :tags (cons cat tags)
                   :body (my/vulpea--article-body preview-url cover-block))))

(defun my/vulpea--capture-concept (node)
  (let* ((title (org-roam-node-title node))
         (aliases (xprompt-aliases)))
    (vulpea-create title "concept/%<%Y%m%d%H%M%S>.org"
                   :properties (my/vulpea-props :type "concept"
                                                :aliases aliases)
                   :tags (my/roam-tag-list)
                   :body my/vulpea--typical-body)))

(defun my/vulpea--capture-idea (node)
  (let* ((title (org-roam-node-title node))
         (aliases (xprompt-aliases))
         (cat (my/pick-tags "idea" "Type of idea"))
         (subcat (when (string= cat "project")
                   (my/pick-tags "project" "Type of project")))
         (tags (list cat subcat)))
    (vulpea-create title "idea/%<%Y%m%d%H%M%S>.org"
                   :properties (my/vulpea-props :type "idea"
                                                :cat cat
                                                :aliases aliases)
                   :tags (append tags (my/roam-tag-list))
                   :body  "* meta\n* summary\n* details\n%?")))

(defun my/vulpea--capture-person (node)
  (let* ((title (org-roam-node-title node))
         (aliases (xprompt-aliases))
         (cat (my/pick-tags "person" "Type of person"))
         (tags (my/roam-tag-list)))
    (vulpea-create title "person/%<%Y%m%d%H%M%S>.org"
                   :properties (my/vulpea-props :type "person"
                                                :cat cat
                                                :aliases aliases)
                   :tags (cons cat tags)
                   :body my/vulpea--typical-body)))

(setq my/vulpea--quote-upper-body-t "
* meta
* summary
* details
#+begin_quote
%?
#+end_quote

")

(defun my/vulpea--quote-body (&optional url)
  (let* ((upper my/vulpea--quote-upper-body-t)
         (src "Source: %s\n\n")
         (lower (if (xnil-or-blank url)
                    (org-link-make-string "cite:&${my/pick-bibtex-key}") ; cite
                  (format src (org-link-make-string
                               url (xstr-default (xprompt "Source name") "link")))))) ; url
    (concat upper lower)))

(defun my/vulpea--capture-quote (node)
  (let* ((title (org-roam-node-title node))
         (cat (my/pick-tags "quote" "Type of quote"))
         (tags (my/roam-tag-list))
         (url (when (not (string= cat "literature"))
                (xprompt-url "Quote URL")))) ; not required, we prompt for a cite key otherwise
    (vulpea-create title "quote/%<%Y%m%d%H%M%S>.org"
                   :properties (my/vulpea-props :type "quote"
                                                :cat cat
                                                :roamrefs url)
                   :tags (cons cat tags)
                   :body (my/vulpea--quote-body url))))

;; credit to nobiot
(defvar my/capture-switch)
(setq my/capture-switch '((?a "article" my/vulpea--capture-article)
                          (?c "concept" my/vulpea--capture-concept)
                          (?i "idea" my/vulpea--capture-idea)
                          (?p "person" my/vulpea--capture-person)
                          (?q "quote" my/vulpea--capture-quote)))
