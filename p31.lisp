;;;; (**) Determine whether a given integer number is prime.
;;;; Example:
;;;; * (is-prime 7)
;;;; T

(require :lisp-unit)

(defun p31-is-prime (n)
  (loop for i upfrom 2 until (> i (sqrt n))
     never (= 0 (rem n i))))

(lisp-unit:define-test is-prime-with-primes
  (loop for prime in '(2 3 5 7 11 23 104729 611953)
     do (lisp-unit:assert-true (p31-is-prime prime))))

(lisp-unit:define-test is-prime-with-composites
  (loop for composite in '(4 6 8 10 187 104730 611954)
     do (lisp-unit:assert-false (p31-is-prime composite))))
