;; -*- lexical-binding: t; -*-
(require 'neutron)
(neutron--setup-todo-keywords)

(map! "M-t" (cmd! (neutron-create-task t))
      :leader
      :prefix "j"
      :desc "Create global task"         "T" #'neutron-create-task
      :desc "Create habit"               "h" #'neutron-create-habit
      :desc "Create local task"          "t" (cmd! (neutron-create-task t))
      :desc "Create project"             "c" #'neutron-create-project
      :desc "Create sibling"             "s" #'neutron-create-sibling
      :desc "Delete project"             "d" #'neutron-delete-project
      :desc "Move project"               "m" #'neutron-move-project
      :desc "Refile tasks"               "k" #'neutron-refile-tasks
      :desc "Rename project"             "R" #'neutron-rename-project
      :desc "Set global project status"  "L" (cmd! (neutron-set-project-status t))
      :desc "Set local project status"   "l" #'neutron-set-project-status)
