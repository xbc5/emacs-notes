;;; ../projects/linux/emacs-notes/group/rust.el -*- lexical-binding: t; -*-

(use-package! rust-mode
              :hook (rust-mode-hook . (setq indent-tabs-mode nil))
              :config
              (setq rust-format-on-save t))
