;;;; (**) Determine the prime factors of a given positive integer.
;;;;
;;;; Construct a flat list containing the prime factors in ascending
;;;; order.
;;;;
;;;; Example:
;;;; * (prime-factors 315)
;;;; (3 3 5 7)

(require :lisp-unit)

(defun p35-prime-factors (n)
  (labels ((recur (m k)
	     (cond ((= m 1) '())
		   ((and (p31-is-prime k) (= (rem m k) 0))
		    (cons k (recur (/ m k) k)))
		   (t (recur m (1+ k))))))
    (recur n 2)))

(lisp-unit:define-test p35-prime-factors-known-inputs
  (lisp-unit:assert-equal '(3 3 5 7) (p35-prime-factors 315)))

(lisp-unit:define-test p35-prime-factors-test
  (loop for i from 2 upto 1000
     do (lisp-unit:assert-equal i (apply #'* (p35-prime-factors i)))))
