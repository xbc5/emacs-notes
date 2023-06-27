(setq smenu--yes-no
      '((?y "yes" t)
        (?n "no" nil)))

;; credit to nobiot
(defun smenu--prompt (switch)
  "Present a keymap prompt."
  (mapconcat
   (pcase-lambda (`(,key ,label))
     (format "[%s] %s"
             (propertize (key-description `(,key)) 'face 'bold)
             label))
   switch "  "))

;; credit to nobiot
(defun smenu (prompt switch)
  "Take a switch, prompt the user to make a choice, then
return the result from the switch.

The switch takes a list whose items take the form of
`(?key \"desc\" fn ...rest)`. The function takes any
interface that you wish. ...rest can be anything that
you want to provide context (e.g. a plist). Which may
allow you to define a context, and do conditional
dispatch -- for example a plist with tags, that tells
the caller to pass specific arguments, or expect a
specific return value. Generally you should give all
of your functions the same interface, but this is an
option.

The return value looks like: `(fn ...rest)`. "
  (nthcdr 2 (assq (read-event
                   (concat prompt ": "
                           (smenu--prompt switch))) switch)))

(defun smenu-confirm (prompt)
  "A simple yes or no prompt. Returns t
if YES, nil if NO."
  (nth 0 (smenu prompt smenu--yes-no)))
