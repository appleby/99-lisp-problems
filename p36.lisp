;;;; (**) Determine the prime factors of a given positive integer (2).
;;;;
;;;; Construct a list containing the prime factors and their
;;;; multiplicity.
;;;;
;;;; Example:
;;;; * (prime-factors-mult 315)
;;;; ((3 2) (5 1) (7 1))
;;;; Hint: The problem is similar to problem P13.

(require :lisp-unit)
(load "p35.lisp")

(defun p36-prime-factors-mult (n)
  (loop
     with count = 1
     for (k . primes) on (p35-prime-factors n)
     when (eql k (car primes)) do (incf count)
     else collect (list k count)
     and do (setf count 1)))
       
(lisp-unit:define-test p36-prime-factors-mult-known-inputs
  (lisp-unit:assert-equal '((3 2) (5 1) (7 1)) (p36-prime-factors-mult 315)))

(lisp-unit:define-test p36-prime-factors-mult-test
  (loop for i from 2 below 1000
     do (lisp-unit:assert-eql i (apply #'* (mapcar (lambda (x) (apply #'expt x))
						   (p36-prime-factors-mult i))))))
