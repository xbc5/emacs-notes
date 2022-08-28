;;; ../projects/linux/emacs-notes/packages/plantutl-mode.el -*- lexical-binding: t; -*-

(use-package! plantuml-mode
              :after (org)
              :config
              (setq plantuml-default-exec-mode 'executable                              ;; jar|executable
                    plantuml-jar-path "/usr/share/java/plantuml.jar"                    ;; used directly by *.plantuml files
                    org-plantuml-jar-path "/usr/share/java/plantuml.jar"                ;; this..
                    org-plantuml-executable-path "/usr/bin/plantuml")                   ;;   or this is redundant. who cares?
              (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))                ;; specify plantuml language for org mode
              (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))        ;; enable mode automatically for *.plantuml files
              (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))) ;; load plantuml language for babel source blocks
              (add-to-list 'org-structure-template-alist
               '("p" . "src plantuml :file img/default.png\n@startuml\n\n@enduml")))
