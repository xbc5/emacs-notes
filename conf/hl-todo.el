;;; ../projects/linux/emacs-notes/group/general.el -*- lexical-binding: t; -*-

;; FIXME: I'm unable to get it working for org-mode
(use-package! hl-todo
              :config
              (add-to-list 'hl-todo-keyword-faces '("WARN" error bold))
              (add-to-list 'hl-todo-keyword-faces '("WARNING" error bold))
              (add-to-list 'hl-todo-include-modes 'org-mode)
              (setq hl-todo-exclude-modes nil           ;; orgmode excluded by default
                    global-hl-todo-mode t))
