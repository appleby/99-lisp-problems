;;;; (**) Flatten a nested list structure.
;;;; Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
;;;; Example:
;;;; * (my-flatten '(a (b (c d) e)))
;;;; (A B C D E)
;;;; Hint: Use the predefined functions list and append.
(in-package :99)

(defun flatten (lst)
  (cond
    ((null lst) '())
    ((atom lst) (list lst))
    (t (append (flatten (car lst)) (flatten (cdr lst))))))

(define-test flatten-test
  (assert-equal '() (flatten '()))
  (assert-equal '(a b c d e) (flatten '(a (b (c d) e)))))
