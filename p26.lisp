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
(in-package :99-problems)

(defun combination (k lst)
  (if (= k 1)
      (loop for e in lst collect (list e))
      (loop for ll on lst
	 while (>= (length ll) k)
	 append (loop for c in (combination (1- k) (cdr ll))
		      collect (cons (car ll) c)))))

(define-test combination-test
  (assert-equal '() (combination 1 '()))
  (assert-equal '((a) (b) (c)) (combination 1 '(a b c)))
  (assert-equality #'tree-equal
		   '((a b) (a c) (a d) (b c) (b d) (c d))
		   (combination 2 '(a b c d)))
  (assert-equality #'tree-equal
		   '((a b c) (a b d) (a b e) (a b f) (a c d) (a c e) (a c f)
		     (a d e) (a d f) (a e f) (b c d) (b c e) (b c f) (b d e)
		     (b d f) (b e f) (c d e) (c d f) (c e f) (d e f))
		   (combination 3 '(a b c d e f))))
