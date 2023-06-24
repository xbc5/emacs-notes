;; Credit to nobiot for some of these functions:
;;  https://org-roam.discourse.group/t/tips-how-to-combine-org-roam-capture-org-capture-and-other-commands-into-a-single-menu-prompt/1262
;;
;; interactive
;; (cons 1 '(2)) aka consing
;; string-match
(require 'vulpea)

(use-package! vulpea
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))

(defun my/vulpea--article-body (preview-url cover-block)
  (let* ((head "* details\n%?\n* media\n"))
    (unless (string-blank-p cover-block)
      (setf head (concat head (format "%s\n\n" cover-block))))
    (unless (string-blank-p preview-url)
      (setf head (concat head (format "[[%s][Preview]]\n" preview-url))))
    head))

(setq my/vulpea--typical-body "* meta\n* summary\n* details\n%?\n* conclusion\n")

;; TODO: add message to menu: e.g. Download source:
;; TODO: add [u]se option on file conflict
;; TODO: do URL embeds
(defun my/vulpea--capture-article (title)
  (interactive "sTitle: ")
  (let* ((aliases (my/prompt-for-aliases))
         (cat (my/pick-tags "article" "Article category"))
         (preview-url  (when (my/tags-p "needs-preview" cat)
                         (my/prompt "Preview URL")))
         (cover-block  (when (my/tags-p "needs-cover" cat)
                         (my/img-block-prompt "cover" title "Cover URL")))
         (rating  (when (my/tags-p "needs-rating" cat)
                    (my/rating-prompt)))
         (year  (when (my/tags-p "needs-year" cat)
                  (my/year-prompt)))
         (state  (when (my/tags-p "needs-state" cat)
                   (my/pick-tags "state" "Article state")))
         (tags (my/roam-tag-list)))
    (vulpea-create title "article/%<%Y%m%d%H%M%S>.org"
                   :context (list :aliasx aliases :cat cat :rating rating :state state :year year)
                   :properties (my/vulpea-props :type "article"
                                                :cat "${cat}"
                                                :aliases "${aliasx}"
                                                :state "${state}"
                                                :year "${year}"
                                                :rating "${rating}")
                   :tags (cons cat tags)
                   :body (my/vulpea--article-body preview-url cover-block))))

(defun my/vulpea--capture-concept (title)
  (interactive "sTitle: ")
  (let* ((aliases (my/prompt-for-aliases)))
    (vulpea-create title "concept/%<%Y%m%d%H%M%S>.org"
                   :context (list :aliasx aliases)
                   :properties (my/vulpea-props :type "concept"
                                                :aliases "${aliasx}")
                   :tags (my/roam-tag-list)
                   :body my/vulpea--typical-body)))

(defun my/vulpea--capture-idea (title)
  (interactive "sTitle: ")
  (let* ((aliases (my/prompt-for-aliases))
         (cat (my/pick-tags "idea" "Type of idea")))
    (vulpea-create title "idea/%<%Y%m%d%H%M%S>.org"
                   :context (list :aliasx aliases :cat cat)
                   :properties (my/vulpea-props :type "idea"
                                                :cat "${cat}"
                                                :aliases "${aliasx}")
                   :tags (my/roam-tag-list)
                   :body  "* meta\n* summary\n* details\n%?")))

(defun my/vulpea--capture-lit (title)
  (interactive "sTitle: ")
  (vulpea-create title "lit/%<%Y%m%d%H%M%S>.org"
                 :properties (my/vulpea-props :type "literature"
                                              :roamrefs "cite:${citekey}")
                 :tags (my/roam-tag-list)
                 :body my/vulpea--typical-body))

(defun my/vulpea--capture-person (title)
  (interactive "sTitle: ")
  (let* ((aliases (my/prompt-for-aliases))
         (cat (my/pick-tags "person" "Type of person"))
         (tags (my/roam-tag-list)))
    (vulpea-create title "person/%<%Y%m%d%H%M%S>.org"
                   :context (list :aliasx aliases :cat cat)
                   :properties (my/vulpea-props :type "person"
                                                :cat "${cat}"
                                                :aliases "${aliasx}")
                   :tags (cons cat tags)
                   :body my/vulpea--typical-body)))

;; credit to nobiot
(defvar my/capture-switch)
(setq my/capture-switch '((?a "article" (lambda (title) (my/vulpea--capture-article title)))
                          (?c "concept" (lambda (title) (my/vulpea--capture-concept title)))
                          (?i "idea" (lambda (title) (my/vulpea--capture-idea title)))
                          (?l "literature" (lambda (title) (my/vulpea--capture-lit title)))
                          (?p "person" (lambda (title) (my/vulpea--capture-person title)))))
