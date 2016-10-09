;;;; (*) Find the last box of a list.
;;;; Example:
;;;; * (my-last '(a b c d))
;;;; (D)
(in-package :99-problems)

(defun my-last (lst)
  (if (null (cdr lst))
      lst
      (my-last (cdr lst))))

(define-test my-last-test
  (assert-equal '(d) (my-last '(a b c d))))
