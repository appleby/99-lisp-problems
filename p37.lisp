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

(require :lisp-unit)

(load "p34.lisp")
(load "p36.lisp")

;;; Note that there is an error in the equation given for phi,
;;; above. The series should be a product, not a sum.
(defun p37-totient-phi (n)
  (apply #'* (loop for (p m) in (p36-prime-factors-mult n)
		collect (* (1- p) (expt p (1- m))))))

(lisp-unit:define-test p37-totient-phi-test
  (loop for i from 2 upto 1000
     do (lisp-unit:assert-eql (p34-totient-phi i) (p37-totient-phi i))))
