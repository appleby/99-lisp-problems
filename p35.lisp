;;;; (**) Determine the prime factors of a given positive integer.
;;;;
;;;; Construct a flat list containing the prime factors in ascending
;;;; order.
;;;;
;;;; Example:
;;;; * (prime-factors 315)
;;;; (3 3 5 7)
(in-package :99)

(defun prime-factors (n)
  (labels ((recur (m k)
	     (cond ((= m 1) '())
		   ((and (is-prime k) (= (rem m k) 0))
		    (cons k (recur (/ m k) k)))
		   (t (recur m (1+ k))))))
    (recur n 2)))

(define-test prime-factors-known-inputs
  (assert-equal '(3 3 5 7) (prime-factors 315)))

(define-test prime-factors-test
  (loop for i from 2 upto 1000
     do (assert-equal i (apply #'* (prime-factors i)))))
