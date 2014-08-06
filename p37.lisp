;;;; (**) Calculate Euler's totient function phi(m) (improved).
;;;;
;;;; See problem P34 for the definition of Euler's totient
;;;; function. If the list of the prime factors of a number m is known
;;;; in the form of problem P36 then the function phi(m) can be
;;;; efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3)
;;;; ...) be the list of prime factors (and their multiplicities) of a
;;;; given number m. Then phi(m) can be calculated with the following
;;;; formula:
;;;;
;;;; phi(m) = (p1 - 1) * p1 ** (m1 - 1) + (p2 - 1) * p2 ** (m2 - 1) +
;;;;          (p3 - 1) * p3 ** (m3 - 1) + ...
;;;;
;;;; Note that a ** b stands for the b'th power of a.
(in-package :99)

;;; Note that there is an error in the equation given for phi,
;;; above. The series should be a product, not a sum.
(defun totient-phi-improved (n)
  (when (not (plusp n))
    (error "~a is not positive." n))
  (apply #'* (loop for (p m) in (prime-factors-mult n)
		collect (* (1- p) (expt p (1- m))))))

(define-test totient-phi-improved-test
  (loop for i from 1 upto 1000
     do (assert-eq (totient-phi i) (totient-phi-improved i))))
