;;;; (*) Generate a random permutation of the elements of a list.
;;;; Example:
;;;; * (rnd-permu '(a b c d e f))
;;;; (B A D C E F)
;;;; Hint: Use the solution of problem P23.

(load "p23.lisp")

(defun p25-rnd-permu (lst)
  (p23-rnd-select lst (length lst)))
