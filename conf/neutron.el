;; -*- lexical-binding: t; -*-
(require 'neutron)

;; Clear default org todo keywords so neutron's are the only ones.
(setq org-todo-keywords nil)


(map! "M-t" (cmd! (neutron-create-task t))
      :leader
      :prefix "j"
      :desc "Create habit"        "h" #'neutron-create-habit
      :desc "Create project"      "c" #'neutron-create-project
      :desc "Create sibling"      "s" #'neutron-create-sibling
      :desc "Create global task"  "T" #'neutron-create-task
      :desc "Create local task"   "t" (cmd! (neutron-create-task t))
      :desc "Delete project"      "d" #'neutron-delete-project
      :desc "Move project"        "m" #'neutron-move-project
      :desc "Refile tasks"        "k" #'neutron-refile-tasks
      :desc "Rename project"      "R" #'neutron-rename-project)
