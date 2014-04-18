;;;; (*) Compare the two methods of calculating Euler's totient
;;;; function.
;;;;
;;;; Use the solutions of problems P34 and P37 to compare the
;;;; algorithms. Take the number of logical inferences as a measure
;;;; for efficiency. Try to calculate phi(10090) as an example.

(load "p34.lisp")
(load "p37.lisp")

(defun p38-compare-totients (n)
  (time (loop repeat n do (p34-totient-phi 10090)))
  (time (loop repeat n do (p37-totient-phi 10090))))
