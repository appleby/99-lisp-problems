;;;; (*) Determine whether two positive integer numbers are coprime.
;;;; Two numbers are coprime if their greatest common divisor equals 1.
;;;; Example:
;;;; * (coprime 35 64)
;;;; T
(in-package :99-problems)

(defun coprime (m n)
  (= (gcd m n) 1))

(define-test coprime-test
  (assert-false (coprime 2 4))
  (assert-false (coprime 3 9))
  (assert-true (coprime 2 3))
  (assert-true (coprime 35 64)))
