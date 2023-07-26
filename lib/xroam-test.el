(require 'el-mock)

(ert-deftest test:xroam--props-merge-one ()
  "It should prompt on singles conflict; merge for multi."
  (let* ((-types xroam--props-tracked))
    (unwind-protect
        (with-mock
          (setq xroam--props-tracked (ht ('foo 'single) ('foos 'multi)))
          (stub prompter-new =>  "new")
          (stub prompter-curr => "curr")
          (mock (prompter 'foo "new" "curr") => "new")
          (should (xroam--props-merge-one 'foo "new" "curr" 'prompter))
          ;; singles => prompt on conflict
          (should (xeq "new"  (xroam--props-merge-one 'foo "new" "curr" 'prompter-new)))
          (should (xeq "curr" (xroam--props-merge-one 'foo "new" "curr" 'prompter-curr)))
          (should (xeq "curr" (xroam--props-merge-one 'foo  nil  "curr" 'prompter-new)))
          (should (xeq "new"  (xroam--props-merge-one 'foo "new"  nil   'prompter-curr)))
          ;; multi => merge
          (should (equal '("1" "2") (xroam--props-merge-one 'foos '("1")     '("2"))))
          (should (equal '("1" "2") (xroam--props-merge-one 'foos '("1" "2") '("2"))))
          (should (equal '("1" "2") (xroam--props-merge-one 'foos '("1")     '("2" "2"))))
          (should (equal '("1" "2") (xroam--props-merge-one 'foos '("2" "1")   nil)))
          (should (equal '("1" "2") (xroam--props-merge-one 'foos   nil      '("2" "1")))))
      (setq xroam--props-tracked -types))))

(ert-deftest test:xroam--props-merge ()
  "Should merge singles and multi."
  (let* ((-types xroam--props-tracked))
    (unwind-protect
        (with-mock
          (stub prompter-new  => "new")
          (stub prompter-curr => "curr")
          (setq xroam--props-tracked (ht ('foo 'single) ('foos 'multi)))
          (should (xht-equal (ht ('foo "new") ('foos '("0" "1")))
                             (xroam--props-merge
                              (ht ('foo "new")  ('foos '("0")))
                              (ht ('foo "curr") ('foos '("1")))
                              'prompter-new)))
          (should (xht-equal (ht ('foo "curr") ('foos '("0" "1")))
                             (xroam--props-merge
                              (ht ('foo "new")  ('foos '("0")))
                              (ht ('foo "curr") ('foos '("1")))
                              'prompter-curr))))
      (setq xroam--props-tracked -types))))

(ert-deftest test:xroam--props-value-parse ()
  "Should fetch singles as \"string\" and multi as '(\"string\"),
applying xneat."
  ;; single
  (with-mock
    (should (xeq
             "foo"
             (xroam--prop-value-parse "foo" 'single)))
    (should (xeq
             "foo bar"
             (xroam--prop-value-parse "  foo   bar  " 'single)))
    (should (xeq
             "\" foo bar \" \" baz \""
             (xroam--prop-value-parse "  \" foo   bar \" \" baz \"   " 'single)))

    ;; multi
    (should (equal '("foo")
                   (xroam--prop-value-parse "foo" 'multi)))
    (should (equal
             '("foo" "bar")
             (xroam--prop-value-parse "\"foo\" \"bar\"" 'multi)))
    (should (equal
             '("foo bar" "baz")
             (xroam--prop-value-parse "\"foo bar\" \"baz\"" 'multi)))
    (should (equal
             '("foo bar" "baz")
             (xroam--prop-value-parse "\" foo   bar \" \"baz\"" 'multi)))))

(defun test:xroam--fake-node ()
  (org-roam-node-create
   :properties '(("S" . "curr") ("M" . "\"curr0\"")))) ;; must be dotted pairs!

(defun test:xroam--fake-node-2 ()
  (org-roam-node-create
   :properties '(("S" . "single") ;; must be dotted pairs!
                 ("M" . "\"multi 0\" \"multi 1\"")
                 ("IGNORED" . "this should be ignored"))))

(ert-deftest test:xroam--props-get ()
  "Should fetch singles as \"string\" and multi as '(\"string\")."
  (let* ((-types xroam--props-tracked)
         (node (test:xroam--fake-node-2)))
    (unwind-protect
        (with-mock
          (setq xroam--props-tracked (ht ('s 'single) ('m 'multi)))
          (should (equal '((s "single") (m ("multi 0" "multi 1")))
                         (xroam--props-get node t))))
      (setq xroam--props-tracked -types))))

(ert-deftest test:xroam--props-merge-all ()
  "Should merge new, existing, and defaults."
  (let* ((-types xroam--props-tracked)
         (node (test:xroam--fake-node)))
    (unwind-protect
        ;; the algorithmic order is: new => current => defaults
        (with-mock
          (setq xroam--props-tracked (ht ('s 'single) ('m 'multi) ('n 'single))) ; n for nil (doesn't exist)
          (stub prompter-new  => "new")
          (stub new-fn => (ht ('s "new") ('m '("new0")))) ; merges with current props
          (should (xht-equal (ht ('s "new") ('m '("curr0" "new0")))
                             (xroam--props-merge-all 'new-fn
                                                     'xecho ; don't default, we don't need to
                                                     (test:xroam--fake-node)
                                                     'prompter-new))))
      (setq xroam--props-tracked -types))))
