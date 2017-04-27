
(ert-deftest dom-mount-text-1 ()
  (with-temp-buffer
    (let ((node "some text"))
      (dom/mount node)
      (should (string-equal "some text" (buffer-string)))
      (let ((root (dom//root-at-point)))
        (should (= (dom-mounted-node-beg root) (point-min)))
        (should (= (dom-mounted-node-end root) (point-max)))
        (should (string-equal (dom-mounted-node-children root) "some text")))
      (should (= (point-min) (point))))))

(ert-deftest dom-mount-text-twice ()
  (with-temp-buffer
    (let ((node "some text"))
      (dom/mount node)
      (dom/mount node)
      (should (string-equal "some text" (buffer-string))))))

(ert-deftest dom-mount-change-text ()
  (with-temp-buffer
    (dom/mount "some other text")
    (dom/mount "some text")
    (should (string-equal "some text" (buffer-string)))))

(ert-deftest dom-mount-change-text-properties ()
  (with-temp-buffer
    (dom/mount "some text")
    (dom/mount (propertize "some text" 'foo "bar"))
    (should (string-equal "some text" (buffer-string)))
    (should (string-equal "bar" (get-text-property (point) 'foo)))))

(ert-deftest dom-mount-empty-text ()
  (with-temp-buffer
    (let ((node ""))
      (dom/mount node)
      (should (string-equal "" (buffer-string))))))

(ert-deftest dom-mount-empty-node ()
  (with-temp-buffer
    (let ((node (dom/node)))
      (dom/mount node)
      (should (string-equal "" (buffer-string))))))

(ert-deftest dom-mount-node-then-text ()
  (with-temp-buffer
    (dom/mount (dom/node "foo"))
    (dom/mount "a")
    (should (string-equal "a" (buffer-string)))))


(ert-deftest dom-mount-node-with-single-text-child ()
  (with-temp-buffer
    (dom/mount (dom/node "some text"))
    (should (string-equal "some text" (buffer-string)))
    (let ((root (dom//root-at-point)))
      (should (= (dom-mounted-node-beg root) (point-min)))
      (should (= (dom-mounted-node-end root) (point-max))))))

(ert-deftest dom-mount-empty-node-then-node-with-single-text-child ()
  (with-temp-buffer
    (dom/mount (dom/node))
    (dom/mount (dom/node "some text"))
    (should (string-equal "some text" (buffer-string)))
    (let ((root (dom//root-at-point)))
      (should (= (dom-mounted-node-beg root) (point-min)))
      (should (= (dom-mounted-node-end root) (point-max))))))

(ert-deftest dom-mount-text-node-then-node-with-single-text-child ()
  (with-temp-buffer
    (dom/mount "some other text")
    (dom/mount (dom/node "some text"))
    (should (string-equal "some text" (buffer-string)))))

(ert-deftest dom-mount-node-with-single-text-child-then-empty-node ()
  (with-temp-buffer
    (dom/mount (dom/node "some text"))
    (dom/mount (dom/node))
    (should (string-equal "" (buffer-string)))))

(ert-deftest dom-mount-node-delete-multiple-extraneous-children ()
  (with-temp-buffer
    (dom/mount (dom/node "some" " " "text"))
    (dom/mount (dom/node "foo"))
    (should (string-equal "foo" (buffer-string)))))

(ert-deftest dom-mount-node-nil ()
  (with-temp-buffer
    (dom/mount "some text")
    (dom/mount nil)
    (should (string-equal "" (buffer-string)))))

(ert-deftest dom-mount-node-nils ()
  (with-temp-buffer
    (dom/mount (dom/node "some" "text"))
    (dom/mount (dom/node nil nil "foo"))
    (should (string-equal "foo" (buffer-string)))))

(ert-deftest dom-mount-node-nils-2 ()
  (with-temp-buffer
    (dom/mount (dom/node nil nil "foo" nil nil nil))
    (should (string-equal "foo" (buffer-string)))))

(ert-deftest dom-mount-node-nils-3 ()
  (with-temp-buffer
    (dom/mount (dom/node "some" "text"))
    (should (string-equal "sometext" (buffer-string)))
    (dom/mount (dom/node nil "text"))
    (should (string-equal "text" (buffer-string)))
    (dom/mount (dom/node "some" "text"))
    (should (string-equal "sometext" (buffer-string)))
    (dom/mount (dom/node nil "text"))
    (should (string-equal "text" (buffer-string)))))

(ert-deftest dom-mount-node-empty-strings ()
  (with-temp-buffer
    (dom/mount (dom/node "some" (dom/node "abc") "text"))
    (dom/mount (dom/node "some" "" "" "text"))
    (should (string-equal "sometext" (buffer-string)))
    (dom/mount (dom/node "some" (dom/node "abc") "text"))
    (should (string-equal "someabctext" (buffer-string)))
    ))

(ert-deftest dom-mount-node-with-line-breaks ()
  (with-temp-buffer
    (dom/mount (dom/node
                (dom/node "aa" "\n")))
    (dom/mount (dom/node
                (dom/node "bbbb" "\n")
                (dom/node "aa" "\n")))
    (should (string-equal "bbbb\naa\n" (buffer-string)))))

(ert-deftest dom-mount-node-structure-1 ()
  (with-temp-buffer
    (dom/mount "some other text")
    (dom/mount (dom/node "some "
                         (dom/node "te")
                         (dom/node (dom/node "xt"))))
    (should (string-equal "some text" (buffer-string)))
    (let ((root (dom//root-at-point)))
      (should (= (dom-mounted-node-beg root) (point-min)))
      (should (= (dom-mounted-node-end root) (point-max))))))

(ert-deftest dom-mount-slightly-changed-structure ()
  (with-temp-buffer
    (dom/mount (dom/node "some "
                         (dom/node "haaa")
                         (dom/node (dom/node "xt"))))
    (dom/mount (dom/node "some "
                         (dom/node "te")
                         (dom/node (dom/node "xt"))))
    (should (string-equal "some text" (buffer-string)))
    (let ((root (dom//root-at-point)))
      (should (= (dom-mounted-node-beg root) (point-min)))
      (should (= (dom-mounted-node-end root) (point-max))))))

(ert-deftest dom-mount-with-properties ()
  (with-temp-buffer
    (dom/mount (dom/with-properties "some text" 'foo "bar"))
    (should (string-equal "some text" (buffer-string)))
    (should (string-equal "bar" (get-text-property (point) 'foo)))
    (let ((root (dom//root-at-point)))
      (should (string-equal "bar" (alist-get 'foo (dom-mounted-node-properties root)))))))

(ert-deftest dom-mount-with-properties-remove ()
  (with-temp-buffer
    (dom/mount (dom/with-properties "some text" 'foo "bar"))
    (dom/mount (dom/node "some text"))
    (should (string-equal "some text" (buffer-string)))
    (should (null (get-text-property (point) 'foo)))))

(ert-deftest dom-mount-delete-in-the-middle ()
  (with-temp-buffer
    (dom/mount (dom/node
                (dom/make-node :children '("foo") :key "a")
                (dom/make-node :children '("bar") :key "b")
                (dom/make-node :children '("baz") :key "c")))
    (put-text-property 7 10 'baz t) ;; to check whether the baz was modified
    (dom/mount (dom/node
                (dom/make-node :children '("foo") :key "a")
                (dom/make-node :children '("baz") :key "c")))
    (should (string-equal "foobaz" (buffer-string)))
    (should (equal-including-properties (propertize "baz" 'baz t)
                                        (buffer-substring 4 7)))))

(ert-deftest dom-sorted-alist-reconcile ()
  (let ((reconciler (lambda (a b) (message "reconciler %s %s" a b) (setcdr b (cdr a)))))
    (should (equal nil (dom//sorted-alist-reconcile! reconciler '< nil nil)))
    (should (equal '((0 . 0)) (dom//sorted-alist-reconcile! reconciler '< '((0 . 0)) nil)))
    (should (equal '((1 . 1)) (dom//sorted-alist-reconcile! reconciler '<
                                                            '((1 . 1)) (list (cons 1 0)))))
    (should (equal '((2 . 2)) (dom//sorted-alist-reconcile! reconciler '<
                                                            '((2 . 2)) (list (cons 2 0)))))
    (should (equal '((3 . 3)) (dom//sorted-alist-reconcile! reconciler '<
                                                            '((3 . 3)) nil)))
    (should (equal nil (dom//sorted-alist-reconcile! reconciler '<
                                                    nil (list (cons 4 0)))))
    (should (equal '((5 . 5) (6 . 6) (8 . 8))
                   (dom//sorted-alist-reconcile! reconciler '<
                                                 '((5 . 5) (6 . 6) (8 . 8))
                                                 (list (cons 5 0) (cons 7 0) (cons 8 0)))))
    ))
