;; require hydra for lv-message and lv-delete-window
;; which allows for messages just above the echo area.

(defvar xquote-interval nil "The time (sec) between displaying quotations.")

(defun xquote-toggle ()
  "Start/stop the xquote job."
  (interactive)
  (if (xquote--started-p)
      (progn
        (xquote--off)
        (message "xquote disabled"))
    (xquote--on)
    (message "xquote enabled")))

(defun xquote-get (&optional pred)
  "Return a unique quote as a string.
PRED is a predicate function that takes
a roam node as its arg -- return t to
include a node into the list of quotes."
  (when (xquote--empty-p) (xquote--init))
  (xquote--fix-msg (pop xquote--list)))

(defun xquote-print (&optional pred)
  "A wrapper around xquote-get to print a quote."
  (lv-message (xquote-get pred)))

(defun xquote-tag-p (node)
  "A predicate function that determines if the
org-roam NODE has the 'xquote' tag"
  (xhas (org-roam-node-tags node) "xquote"))


;; PRIVATE


(defun xquote--map-nodes (nodes)
  "Map NODES from alist (list (title . node) ...) to
a list of titles: (list \"title\" ...)."
  (mapcar (lambda (conscell) (org-roam-node-title (cdr conscell))) nodes))

(defun xquote--init (&optional pred)
  "Set the quote list with new, random values."
  (setq xquote--list
        (xseq-rdm
         (xquote--map-nodes
          (org-roam-node-read--completions (or pred #'xquote-tag-p))))))

(defun xquote--empty-p ()
  "Determine if quotes are empty or unbound."
  (or
   (not (boundp 'xquote--list))
   (not (cl-typep xquote--list 'list))
   (length= xquote--list 0)))

(defun xquote--disabled-p ()
  "Determine if the package disabled or not."
  (and
   (boundp 'xquote-disabled)
   xquote-disabled))

(defun xquote--started-p ()
  "Determine if xquote is running the in the
background."
  (and
   (boundp 'xquote--started)
   xquote--started))

(defun xquote--should-start-p ()
  "Determine if the background process should
start or not. Depends on xquote--disabled too."
  (not (or (xquote--started-p) (xquote--disabled-p))))

(defun xquote--interval ()
  (when (xnil-ish 'xquote-interval)
    (setq xquote-interval 30))
  xquote-interval)

(defun xquote--fix-msg (str)
  "Capitalise the first letter of STR, and add a period."
  (replace-regexp-in-string "\\.?$" "." (s-capitalize (s-trim str))))

(defvar xquote--timer-obj nil "Holds the TIMER object for the background task.
Use this to cancel the running job.")

(defun xquote--on ()
  "Start the xqoute job."
  (setq xquote--timer-obj
        (run-with-timer 30 (xquote--interval) #'xquote-print))
  (setq xquote--started t)
  (setq xquote-disabled nil))

(defun xquote--off ()
  "Stop the xquote job."
  (unless (xnil-ish 'xquote--timer-obj)
    (cancel-timer xquote--timer-obj))
  (lv-delete-window)
  (setq xquote--timer-obj nil)
  (setq xquote--started nil)
  (setq xquote-disabled t))

(when (xquote--should-start-p)
  (xquote--on))
