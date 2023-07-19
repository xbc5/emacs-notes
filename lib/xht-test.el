(ert-deftest test:xht-equal ()
  "It should return t for equal hash tables, and nil otherwise."
  (should (eq t   (xht-equal (ht ('one 1))
                             (ht ('one 1)))))

  (should (eq t   (xht-equal (ht ('one 1) ('two "2"))
                             (ht ('one 1) ('two "2")))))

  (should (eq t   (xht-equal (ht ('one '(1)))
                             (ht ('one '(1))))))

  (should (eq t   (xht-equal (ht ('one (ht ('one+ 1))))
                             (ht ('one (ht ('one+ 1)))))))

  (should (eq nil (xht-equal (ht ('one 1))
                             (ht ('one 2)))))

  (should (eq nil (xht-equal (ht ('one 1))
                             (ht ('two 1)))))

  (should (eq nil (xht-equal (ht ('one 1) ('two "2"))
                             (ht ('one 1) ('two  2)))))

  (should (eq nil (xht-equal (ht ('one '(1)))
                             (ht ('one '(2))))))

  (should (eq nil (xht-equal (ht ('one (ht ('one+ 1))))
                             (ht ('one (ht ('one+ 2)))))))

  (should (eq nil (xht-equal (ht ('one (ht ('one+ 1))))
                             (ht ('one (ht ('two+ 1))))))))


(ert-deftest test:xht-mutate ()
  "It should perform a sequence of commands upon the hash table."
  ;; apply action on one; ignore the other
  (should (xht-equal (ht ('new 1) ('ignore 2))
                     (xht-mutate (ht ('old 1) ('ignore 2))
                                 '((rename old new)))))
  ;; delete; also an action on different keys
  (should (xht-equal (ht ('new 1))
                     (xht-mutate (ht ('old 1) ('del 2))
                                 '((rename old new)
                                   (delete del)))))
  ;; perform multiple actions on same key
  (should (xht-equal (ht)
                     (xht-mutate (ht ('old 1))
                                 '((rename old new)
                                   (delete new))))))
