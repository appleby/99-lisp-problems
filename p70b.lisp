;;;; (*) Check whether a given expression represents a multiway tree
;;;;
;;;; Write a function istree which succeeds if and only if its
;;;; argument is a Lisp expression representing a multiway tree.
;;;;
;;;; Example:
;;;; * (istree '(a (f g) c (b d e)))
;;;; T
(in-package :99-problems)

(defun is-mw-tree (list)
  (cond ((null list) nil)
	((symbolp list) t)
	((listp list)
	 (and (> (length list) 1)
	      (symbolp (car list))
	      (loop for child in (cdr list) always (is-mw-tree child))))
	(t nil)))

(define-test is-mw-tree-test
  (assert-true (is-mw-tree 'a))
  (assert-true (is-mw-tree '(a b)))
  (assert-true (is-mw-tree '(a (f g) c (b d e))))
  (assert-false (is-mw-tree '()))
  (assert-false (is-mw-tree '((a b) c)))
  (assert-false (is-mw-tree '(a)))
  (assert-false (is-mw-tree '(a (b) c))))
