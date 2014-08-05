;;;; (*) Find out whether a list is a palindrome.
;;;; A palindrome can be read forward or backward; e.g. (x a m a x).

;;; Tried to be clever and only iterate over half the list, but the
;;; calls to reverse and length end up making this version O(2.5n),
;;; whereas the the equal/reverse version below is O(2n).
(in-package :99)

(defun palindrome-iter-p (lst)
  (loop
     for x in lst
     for y in (reverse lst)
     for i upto (ceiling (length lst) 2)
     when (not (equalp x y)) return nil
     finally (return t)))

(defun palindrome-p (lst)
  (equal lst (reverse lst)))

(define-test palindrome-p-test
  (dolist (test-fn (list #'palindrome-p #'palindrome-iter-p))
    (assert-true (funcall test-fn '()))
    (assert-true (funcall test-fn '(a)))
    (assert-true (funcall test-fn '(a a)))
    (assert-true (funcall test-fn '(a b a)))
    (assert-true (funcall test-fn '(x a m a x)))
    (assert-false (funcall test-fn '(a b)))
    (assert-false (funcall test-fn '(a b b)))
    (assert-false (funcall test-fn '(x a m n a x)))))
