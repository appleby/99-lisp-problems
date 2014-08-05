;;;; (*) Find the last box of a list.
;;;; Example:
;;;; * (my-last '(a b c d))
;;;; (D)
(in-package :99)

(defun my-last (lst)
  (if (cdr lst)
      (my-last (cdr lst))
      lst))

(define-test my-last-test
  (assert-equal '(d) (my-last '(a b c d))))
