;;;; (*) Find the K'th element of a list.
;;;; The first element in the list is number 1.
;;;; Example:
;;;; * (element-at '(a b c d e) 3)
;;;; C
(in-package :99-problems)

(defun element-at (lst n)
  (nth (1- n) lst))

(define-test element-at-test
  (assert-eq 'c (element-at '(a b c d e) 3)))
