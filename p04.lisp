;;;; (*) Find the number of elements of a list.
(in-package :99-problems)

(defun my-length (lst)
  (if (null lst)
      0
      (1+ (my-length (cdr lst)))))

(define-test my-length-test
  (assert-eq 0 (my-length '()))
  (assert-eq 1 (my-length '(a)))
  (assert-eq 5 (my-length '(a b c d e))))
