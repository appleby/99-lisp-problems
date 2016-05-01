;;;; (*) Find the last but one box of a list.
;;;; Example:
;;;; * (my-but-last '(a b c d))
;;;; (C D)
(in-package :99-problems)

(defun my-but-last (lst)
  (if (cddr lst)
      (my-but-last (cdr lst))
      lst))

(define-test my-but-last-test
  (assert-equal '(c d) (my-but-last '(a b c d))))
