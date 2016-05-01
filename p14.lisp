;;;; (*) Duplicate the elements of a list.
;;;; Example:
;;;; * (dupli '(a b c c d))
;;;; (A A B B C C C C D D)
(in-package :99-problems)

(defun dupli (lst)
  (mapcan (lambda (x) (list x x)) lst))

(define-test dupli-test
  (assert-equal '() (dupli '()))
  (assert-equal '(a a b b c c c c d d) (dupli '(a b c c d))))
