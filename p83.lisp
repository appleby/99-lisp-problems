;;; (**) Construct all spanning trees
;;;
;;; Write a function (s-tree graph) to construct (by backtracking) all
;;; spanning trees of a given graph. With this function, find out how
;;; many spanning trees there are for the graph depicted to the
;;; left. The data of this example graph can be found in the file
;;; p83.dat. When you have a correct solution for the s-tree function,
;;; use it to define two other useful functions: (is-tree graph) and
;;; (is-connected graph). Both are five-minutes tasks!
(in-package :99)

(defvar *p83-graph* '((a b c d e f g h) ((a b) (a d) (b c) (b e) (c e) (d e)
					 (d f) (d g) (e h) (f g) (g h))))

(defun s-tree (graph)
  graph)

(defun is-connected (graph)
  nil)

(defun is-tree (graph)
  nil)

(defun set^3-equal (sss1 sss2)
  "Compare sets of sets of sets. Each set is compared with set-equal."
  (set-equal sss1 sss2 :test (lambda (x y) (set-equal x y :test #'set-equal))))

(define-test set^3-equal-test
  (assert-equality #'set^3-equal '(((a b))) '(((a b))))
  (assert-equality #'set^3-equal '(((a b))) '(((b a))))
  (assert-equality #'set^3-equal
		   '(((a b) (c d)) ((e f) (g h)))
		   '(((d c) (b a)) ((g h) (f e))))
  (assert-false (set^3-equal '(((a b))) '(((b c)))))
  (assert-false (set^3-equal '(((a b) (c d)) ((e f) (g h)))
			     '(((a b) (c d)) ((e f) (g i))))))

(define-test is-connected-test
  (assert-true (is-connected *p83-graph*))
  (assert-true (is-connected '((a b c) (a b) (b c))))
  (assert-false (is-connected '((a b c) (a b)))))

(define-test is-tree-test
  (assert-true (is-tree '((a))))
  (assert-true (is-tree '((a b) (a b))))
  (assert-true (is-tree '((a b c) (a b) (a c))))
  (assert-false (is-tree '((a b c) (a b) (a c) (b c))))
  (assert-false (is-tree *p83-graph*)))

(define-test s-tree-test
  (assert-equal '(()) (s-tree '()))
  (assert-equal '(((a b) (a b))) (s-tree '((a b) (a b))))
  (assert-equality #'set^3-equal
		   '(((a b c) (a b) (b c))
		     ((a b c) (a c) (c b))
		     ((a b c) (a b) (a c)))
		   (s-tree '((a b c) (a b) (a c) (b c)))))
