;;;; (*) Insert an element at a given position into a list.
;;;; Example:
;;;; * (insert-at 'alfa '(a b c d) 2)
;;;; (A ALFA B C D)
(in-package :99-problems)

(defun insert-at (new-elem lst n)
  (cond ((= n 1)
	 (cons new-elem lst))
	((> n (length lst))
	 (append lst (list new-elem)))
	(t (loop for elem in lst
	      for idx upfrom 1
	      if (= idx n) collect new-elem and collect elem
	      else collect elem))))

(define-test insert-at-test
  (assert-equal '(foo) (insert-at 'foo '() 1))
  (assert-equal '(foo a) (insert-at 'foo '(a) 1))
  (assert-equal '(a b c foo) (insert-at 'foo '(a b c) 10))
  (assert-equal '(a alfa b c d) (insert-at 'alfa '(a b c d) 2)))
