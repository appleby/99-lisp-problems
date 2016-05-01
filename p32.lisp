;;;; (**) Determine the greatest common divisor of two positive integer numbers.
;;;; Use Euclid's algorithm.
;;;; Example:
;;;; * (gcd 36 63)
;;;; 9
(in-package :99-problems)

(defun my-gcd (n m)
  (if (zerop m)
      n
      (my-gcd m (mod n m))))

(define-test my-gcd-test
  (assert-eq 0 (my-gcd 0 0))
  (assert-eq 1 (my-gcd 0 1))
  (assert-eq 1 (my-gcd 1 2))
  (assert-eq 2 (my-gcd 2 4))
  (assert-eq 9 (my-gcd 36 63))
  (assert-eq 9 (my-gcd 63 36)))
