;;;; (**) Eliminate consecutive duplicates of list elements.
;;;;
;;;; If a list contains repeated elements they should be replaced with
;;;; a single copy of the element. The order of the elements should
;;;; not be changed.
;;;;
;;;; Example:
;;;; * (compress '(a a a a b c c a a d e e e e))
;;;; (A B C A D E)
(in-package :99)

(defun compress (lst)
  (labels ((recur (lst prev)
	     (cond
	       ((null lst) '())
	       ((equalp (car lst) prev)
		(recur (cdr lst) prev))
	       (t (cons (car lst) (recur (cdr lst) (car lst)))))))
    (recur lst nil)))

(define-test compress-test
  (assert-equal '() (compress '()))
  (assert-equal '(a b c) (compress '(a b c)))
  (assert-equal '(a b c a d e) (compress '(a a a a b c c a a d e e e e))))
