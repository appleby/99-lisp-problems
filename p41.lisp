;;;; (**) A list of Goldbach compositions.
;;;;
;;;; Given a range of integers by its lower and upper limit, print a
;;;; list of all even numbers and their Goldbach composition.
;;;;
;;;; Example:
;;;; * (goldbach-list 9 20)
;;;; 10 = 3 + 7
;;;; 12 = 5 + 7
;;;; 14 = 3 + 11
;;;; 16 = 3 + 13
;;;; 18 = 5 + 13
;;;; 20 = 3 + 17
;;;;
;;;; In most cases, if an even number is written as the sum of two
;;;; prime numbers, one of them is very small. Very rarely, the primes
;;;; are both bigger than say 50. Try to find out how many such cases
;;;; there are in the range 2..3000.
;;;;
;;;; Example (for a print limit of 50):
;;;; * (goldbach-list 1 2000 50)
;;;; 992 = 73 + 919
;;;; 1382 = 61 + 1321
;;;; 1856 = 67 + 1789
;;;; 1928 = 61 + 1867
(in-package :99)

(defun p41-goldbach-list (start end &optional (print-limit 0))
  (let* ((goldbach-start (cond ((<= start 4) 4)
			       ((evenp start) start)
			       (t (1+ start))))
	 (evens (loop for i from goldbach-start upto end by 2 collect i)))
    (loop for e in evens
	 for primes = (p40-goldbach e)
	 if (< print-limit (first primes)) do (format t "~d = ~{~d~^ + ~}~%" e primes))))
