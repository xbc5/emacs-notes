;;; conf/beancount.el -*- lexical-binding: t; -*-
(require 'f)

;; Specify the beancount ledger directory. We need this to store ledger files.
(defun xbeancount-dpath-ledger ()
  (f-join
   (or org-roam-directory "~/org")
   "beancount-ledger"))

(defun xbeancount--dir-filter (comp-candidate)
  "This matches any beancount ledger nodes."
  ;; If the node exists inside of the beancount ledger directory, include it.
  (string= "beancount-ledger"
           (xroam-subdir-get (org-roam-node-file (cdr comp-candidate)))))

(cl-defun xbeancount-node-find ()
  "Create a new ledger file."
  (interactive)
  (org-roam-node-find
   nil nil #'xbeancount--dir-filter nil
   :templates '(("d" "default" plain "%?"
                 :if-new (file+head "beancount-ledger/%<%Y-%m-%d--%H:%M:%S:%N>.org"
                                    "#+title: ${title}\n#+date: %U\n\n")))))
