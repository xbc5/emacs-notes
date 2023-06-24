(defun smenu-prompt (cmd)
  "Present a keymap prompt."
  (mapconcat
   (pcase-lambda (`(,key ,label))
     (format "[%s] %s"
             (propertize (key-description `(,key)) 'face 'bold)
             label))
   cmd "  "))

;; credit to nobiot
(defun smenu-dispatch (cmd)
  (assq (read-event (smenu-prompt cmd)) cmd))
