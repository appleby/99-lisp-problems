;;;; (**) Calculate Euler's totient function phi(m).
;;;;
;;;; Euler's so-called totient function phi(m) is defined as the
;;;; number of positive integers r (1 <= r < m) that are coprime to m.
;;;;
;;;; Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special
;;;; case: phi(1) = 1.
;;;;
;;;; * (totient-phi 10)
;;;; 4
;;;;
;;;; Find out what the value of phi(m) is if m is a prime
;;;; number. Euler's totient function plays an important role in one
;;;; of the most widely used public key cryptography methods (RSA). In
;;;; this exercise you should use the most primitive method to
;;;; calculate this function (there are smarter ways that we shall
;;;; discuss later).
(in-package :99-problems)

(defun totient-phi (m)
  (when (not (plusp m))
    (error "~a is not positive" m))
  (if (= m 1)
      1
      (loop for i from 1 below m count (coprime m i))))

(define-test totient-phi-test
  (assert-eq 1 (totient-phi 1))
  (assert-eq 4 (totient-phi 10))
  (assert-eq 40 (totient-phi 100))
  (let ((rnd-prime (car (rnd-select (prime-range 2 100) 1))))
    (assert-eq (1- rnd-prime) (totient-phi rnd-prime) rnd-prime))
  (assert-error 'simple-error (totient-phi 0)))
