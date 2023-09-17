(after! org
  (map! "M-q" #'xdrill-question-add
        "M-a" #'xdrill-answer-add
        "M-c" #'xdrill-add ; create a new flashcard file, or pick one
        "M-f" #'xdrill-run))
