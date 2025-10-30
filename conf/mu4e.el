;;; conf/mu4e.el -*- lexical-binding: t; -*-

;; Loads untracked private configuration values.
(my/email--load-config-file)

(after! mu4e
        ;; Auto-updates
        (setq mu4e-update-interval 30)

        ;; Make the mu4e buffer split 50/50 using Emacs display-buffer-alist rules.
        ;; It splits the the mail "view-buffer". Note that `mu4e-split-view 'vertical`
        ;; will override this behaviour and cause the split to take up 80% of the window width.
        (add-to-list 'display-buffer-alist
                     `(,(regexp-quote mu4e-view-buffer-name)
                        display-buffer-in-side-window
                        (side . right)
                        (window-width . 0.5))))
