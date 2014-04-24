;;;; (*) A list of prime numbers.
;;;;
;;;; Given a range of integers by its lower and upper limit, construct
;;;; a list of all prime numbers in that range.
(in-package :99)

(defun p39-prime-range (start end)
  (loop for i from start upto end
     when (p31-is-prime i) collect i))