;;;; (*) Find the K'th element of a list.
;;;; The first element in the list is number 1.
;;;; Example:
;;;; * (element-at '(a b c d e) 3)
;;;; C
(in-package :99-problems)

(defun element-at (lst n)
  (assert (plusp n))
  (cond ((null lst) nil)
	((= 1 n) (car lst))
	(t (element-at (cdr lst) (1- n)))))

(define-test element-at-test
  (assert-eq 'c (element-at '(a b c d e) 3)))
