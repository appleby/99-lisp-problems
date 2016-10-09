;;;; (*) Find the number of elements of a list.
(in-package :99-problems)

(defun my-length (lst)
  (my-length-iter lst 0))

(defun my-length-iter (lst n)
  (if (null lst)
      n
      (my-length-iter (cdr lst) (1+ n))))

(define-test my-length-test
  (assert-eq 0 (my-length '()))
  (assert-eq 1 (my-length '(a)))
  (assert-eq 5 (my-length '(a b c d e))))
