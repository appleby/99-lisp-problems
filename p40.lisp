;;;; (**) Goldbach's conjecture.
;;;;
;;;;
;;;; Goldbach's conjecture says that every positive even number
;;;; greater than 2 is the sum of two prime numbers. Example: 28 = 5 +
;;;; 23. It is one of the most famous facts in number theory that has
;;;; not been proved to be correct in the general case. It has been
;;;; numerically confirmed up to very large numbers (much larger than
;;;; we can go with our Prolog system). Write a predicate to find the
;;;; two prime numbers that sum up to a given even integer.
;;;;
;;;; Example:
;;;; * (goldbach 28)
;;;; (5 23)
(in-package :99)

(defun goldbach (n)
  (let* ((primes (prime-range 2 (- n 2)))
	 (reverse-primes (reverse primes)))
    (loop for p1 in primes do
	 (loop for p2 in reverse-primes
	    for sum = (+ p1 p2)
	    until (or (< sum n) (< p2 p1))
	    when (= sum n) do (return-from goldbach (list p1 p2))))))

(define-test goldbach-known-inputs
  (assert-equal '(5 23) (goldbach 28)))

(define-test goldbach-test
  (loop for i from 4 upto 1000 by 2
     do (let ((result (goldbach i)))
	  (assert-true (every #'is-prime result))
	  (assert-eql i (apply #'+ result)))))
