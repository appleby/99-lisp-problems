;;;; (**) Generate the combinations of K distinct objects chosen from the N elements of a list

;;;; In how many ways can a committee of 3 be chosen from a group of
;;;; 12 people? We all know that there are C(12,3) = 220 possibilities
;;;; (C(N,K) denotes the well-known binomial coefficients). For pure
;;;; mathematicians, this result may be great. But we want to really
;;;; generate all the possibilities in a list.

;;;; Example:
;;;; * (combination 3 '(a b c d e f))
;;;; ((A B C) (A B D) (A B E) ... )


;;; Base case
;;;   The 1-combination is just a list of single-element lists for
;;;   each element of the original list. E.g.
;;;   C(1, {x0, x1, ..., xn}) = {{x0}, {x1}, ..., {xn}}
;;;
;;; Inductive step
;;;   C(k+1, {x0, x1, ..., xn}) =
;;;     {xi ++ c | for xi in {x0, ..., x(n-k)} for c in C(k, {x(i+1), .., xn})}
(in-package :99)
 
(defun p26-combination (k lst)
  (if (= k 1)
      (loop for e in lst collect (list e))
      (loop for ll on lst
	 while (>= (length ll) k)
	 append (loop for c in (p26-combination (1- k) (cdr ll))
		      collect (cons (car ll) c)))))
