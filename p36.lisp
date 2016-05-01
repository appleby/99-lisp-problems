;;;; (**) Determine the prime factors of a given positive integer (2).
;;;;
;;;; Construct a list containing the prime factors and their
;;;; multiplicity.
;;;;
;;;; Example:
;;;; * (prime-factors-mult 315)
;;;; ((3 2) (5 1) (7 1))
;;;; Hint: The problem is similar to problem P13.
(in-package :99-problems)

(defun prime-factors-mult (n)
  (loop
     with count = 1
     for (k . primes) on (prime-factors n)
     when (eql k (car primes)) do (incf count)
     else collect (list k count)
     and do (setf count 1)))
       
(define-test prime-factors-mult-known-inputs
  (assert-equal '((3 2) (5 1) (7 1)) (prime-factors-mult 315)))

(define-test prime-factors-mult-test
  (loop for i from 2 below 1000
     do (assert-eql i (apply #'* (mapcar (lambda (x) (apply #'expt x))
						   (prime-factors-mult i))))))
