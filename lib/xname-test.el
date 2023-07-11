(require 'el-mock)

(ert-deftest xname--test-append-num ()
  "It should truncate and preserve the extension."
  (let* ((len xname-max-len))
    (unwind-protect
        (progn
          (setq xname-max-len 6)
          (should (string= "foo(1)" (xname--append-num "foo" 1)))
          (should (string= "foo(2)" (xname--append-num "foo" 2)))
          (should (string= "fo(10)" (xname--append-num "foo" 10)))
          (setq xname-max-len 10)
          (should (string= "foo(1).ext" (xname--append-num "foo.ext" 1)))
          (should (string= "foo(2).ext" (xname--append-num "foo.ext" 2)))
          (should (string= "fo(10).ext" (xname--append-num "foo.ext" 10))))
      (setq xname-max-len len))))

(ert-deftest xname--test-find ()
  "It should cycle through names until it finds suitable one;
respect the max name length; preserve the extension."
  (let* ((len xname-max-len))
    (unwind-protect
        (progn
          (setq xname-max-len 8)
          (should (string= "foo" (xname--find "/tmp" "foo" (lambda (_) nil))))
          (should (string= "foo(1)" (xname--find "/tmp" "foo"
                                                  (lambda (n)
                                                    (string= n "/tmp/foo")))))
          (should (string= "foo(2)" (xname--find "/tmp" "foo"
                                                  (lambda (n)
                                                    (member n '("/tmp/foo" "/tmp/foo(1)"))))))
          (should (string= "f(1).ext" (xname--find "/tmp" "foobar.ext"
                                                    (lambda (n)
                                                      (string= n "/tmp/foobar.ext"))))))
      (setq xname-max-len len))))

(ert-deftest xname--test-get ()
  "It should return a full, absolute path to NEW:
namified, and truncated."
  (let* ((len xname-max-len))
    (unwind-protect
        (with-mock
          (stub file-checker-f => nil)
          (setq xname-max-len 6)
          (should (string= "/dir/new" (xname--get "/dir" "old" "new" #'file-checker-f)))
          (should (string= "/dir/foo" (xname--get "/dir" "foo" "foo" #'file-checker-f)))
          (should (string= "/dir/foobar" (xname--get "/dir" "old" "foo$bar" #'file-checker-f)))
          (should (string-match-p "^/home/[^/]+/new$"
                                  (xname--get "~/" "old" "new" #'file-checker-f)))
          (should (string= "/dir/foobar" (xname--get "/dir" "old" "foobar1" #'file-checker-f))))

      (setq xname-max-len len))))

(ert-deftest xname--test-set-visited ()
  "It should set the NEW name, applying the necessary name manipulation."
  (let* ((len xname-max-len))
    (unwind-protect
        (with-mock
          (mock (set-new "/dir/new" nil t))
          (stub nothing)
          (stub f-not-exists => nil)
          (setq xname-max-len 6)
          ;; input matches existing visited file name
          (should (string= "/dir/foo" (xname--set-visited
                                       "foo"
                                       (lambda () "" "/dir/foo")
                                       #'nothing
                                       #'f-not-exists)))
          ;; matches input AFTER namify
          (should (string= "/dir/foo" (xname--set-visited
                                       "f$oo"
                                       (lambda () "" "/dir/foo")
                                       #'nothing
                                       #'f-not-exists)))
          ;; input is different from existing visited file name
          (should (string= "/dir/new" (xname--set-visited
                                       "new"
                                       (lambda () "" "/dir/old")
                                       #'set-new
                                       #'f-not-exists)))
          ;; test that it truncates
          (should (string= "/dir/newnew" (xname--set-visited
                                          "newnewX"
                                          (lambda () "" "/dir/old")
                                          #'nothing
                                          #'f-not-exists)))
          ;; test that it renames on conflict
          (should (string= "/dir/new(1)" (xname--set-visited
                                          "new"
                                          (lambda () "" "/dir/old")
                                          #'nothing
                                          (lambda (p) (string= p "/dir/new")))))
          ;; test that it namifies
          (should (string= "/dir/new" (xname--set-visited
                                       "n$ew"
                                       (lambda () "" "/dir/old")
                                       #'nothing
                                       #'f-not-exists))))
      (setq xname-max-len len))))

(defun test--err-not-match (str pat)
  (unless (string-match-p pat str)
    (error "Pattern doesn't match: str: %s; pat: %s" str pat)))

(ert-deftest xname--test-change ()
  "It should rename multiple files."
  (let* ((len xname-max-len)
         (renamer-called nil)
         (old-path "")
         (new-path ""))
    (unwind-protect
        (with-mock
          (mock (rename-1 "/dir/old" "/dir/new"))
          (mock (rename-foo-x "/dir/fooX" "/dir/bar(1)"))
          (mock (rename-special "/dir/foo" "/dir/foobar"))
          (mock (rename-ext "/dir/foo.org" "/dir/ba.org"))
          (mock (rename-ext-x "/dir/foo.x" "/dir/b(1).x"))
          (stub f-not-exists => nil)
          (setq xname-max-len 6)
          ;; straight rename
          (xname-change-all '("/dir/old")
                        (lambda (path) "" "new")
                        #'rename-1
                        #'f-not-exists)
          ;; renames multiple (WIP; not perfect, don't care)
          ;; could test the return value but as of now it doesn't return
          ;; anything useful.
          (xname-change-all '("/dir/one" "/dir/two")
                        (lambda (path) (concat path "1"))
                        (lambda (old new)
                          (print (member old '("/dir/one" "/dir/two")))
                          (xcheck (cons "Unexpected old path" old)
                                  (member old (list "/dir/one" "/dir/two")))
                          (setq old-path old)
                          (xcheck (cons "Unexpected new path" new)
                                  (member new (list "/dir/one1" "/dir/two1")))
                          (setq new-path new))
                        #'f-not-exists)
          (setq new-path "") ; reset
          (setq old-path "")
          ;; same paths
          (xname-change-all '("/dir/foo")
                        (lambda (path) "" "foo")
                        (lambda (old new) (setq renamer-called "same paths")) ; shouldn't call
                        #'f-not-exists)
          ;; same paths
          (xname-change-all '("/dir/fooX")
                        (lambda (path) "" "barX")
                        #'rename-foo-x
                        (lambda (path) (string= path "/dir/barx"))) ; is namified first (downcase)
          ;; namifies properly
          (xname-change-all '("/dir/foo")
                        (lambda (path) "" "foo$bar")
                        #'rename-special
                        #'f-not-exists)
          ;; preserves extension
          (xname-change-all '("/dir/foo.org")
                        (lambda (path) "" "barX")
                        #'rename-ext
                        #'f-not-exists
                        t)
          ;; preserves extension on conflict
          (xname-change-all '("/dir/foo.x")
                        (lambda (path) "" "bar")
                        #'rename-ext-x
                        (lambda (path) (string= path "/dir/bar.x"))
                        t))
      (setq xname-max-len len))))
