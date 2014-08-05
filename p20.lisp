;;;; (*) Remove the K'th element from a list.
;;;; Example:
;;;; * (remove-at '(a b c d) 2)
;;;; (A C D)
(in-package :99)

(defun remove-at (lst n)
  (loop for elem in lst
        for idx upfrom 1
        unless (= idx n) collect elem))

(define-test remove-at-test
  (assert-equal '() (remove-at '() 1))
  (assert-equal '(a c d) (remove-at '(a b c d) 2)))
