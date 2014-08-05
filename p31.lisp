;;;; (**) Determine whether a given integer number is prime.
;;;; Example:
;;;; * (is-prime 7)
;;;; T
(in-package :99)

(defun is-prime (n)
  (if (< n 2)
      nil
      (loop for i upfrom 2 until (> i (sqrt n))
	 never (= 0 (rem n i)))))

(define-test is-prime-with-primes
  (loop for prime in '(2 3 5 7 11 23 104729 611953)
     do (assert-true (is-prime prime))))

(define-test is-prime-with-non-primes
  (loop for non-prime in '(0 1 4 6 8 10 187 104730 611954)
     do (assert-false (is-prime non-prime))))
