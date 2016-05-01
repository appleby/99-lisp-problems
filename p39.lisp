;;;; (*) A list of prime numbers.
;;;;
;;;; Given a range of integers by its lower and upper limit, construct
;;;; a list of all prime numbers in that range.
(in-package :99-problems)

(defun prime-range (start end)
  (loop for i from start upto end
     when (is-prime i) collect i))

(define-test prime-range-test
  (assert-equal '() (prime-range 0 1))
  (assert-equal '() (prime-range 8 9))
  (assert-equal '(2) (prime-range 2 2))
  (assert-equal '(2 3 5 7 11 13 17 19) (prime-range 1 20)))
