;;;; (*) Check whether a given expression represents a binary tree
;;;;
;;;; Write a function istree which returns true if and only if its
;;;; argument is a list representing a binary tree.
;;;;
;;;; Example:
;;;; * (istree '(a (b nil nil) nil))
;;;; T
;;;; * (istree '(a (b nil nil)))
;;;; NIL

(in-package :99)

(defun p54a-istree (lst)
  (or (null lst)
      (and (= 3 (length lst))
	   (loop for c in (remove-if #'atom lst)
	      always (p54a-istree c)))))


(define-test p54a-istree-known-inputs
  (loop for tree in (list *t1* *t2* *t3*)
     do (assert-true (p54a-istree tree)))
  (assert-false (p54a-istree '(a (b nil nil)))))
