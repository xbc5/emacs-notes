(use-package! plantuml-mode
  :after (org)
  :config
  (setq plantuml-default-exec-mode 'executable
        plantuml-jar-path "/usr/share/java/plantuml.jar"
        org-plantuml-jar-path "/usr/share/java/plantuml.jar"
        org-plantuml-executable-path "/usr/bin/plantuml")
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))) ; load plantuml language for babel source blocks
  (add-to-list 'org-structure-template-alist
               '("p" . "src plantuml :file img/default.png\n@startuml\n\n@enduml")))
