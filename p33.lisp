;;;; (*) Determine whether two positive integer numbers are coprime.
;;;; Two numbers are coprime if their greatest common divisor equals 1.
;;;; Example:
;;;; * (coprime 35 64)
;;;; T
(in-package :99)

(defun p33-coprime (m n)
  (= (gcd m n) 1))
