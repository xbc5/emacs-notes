(setq doom-font (font-spec :family "monospace" :size 15 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 13)
      doom-theme 'doom-one
      display-line-numbers-type t)

(map! :desc "completion-at-point" "<M-SPC>" #'completion-at-point)
