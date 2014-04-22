;;;; (*) Split a list into two parts; the length of the first part is
;;;; given. Do not use any predefined predicates.
;;;; Example:
;;;; * (split '(a b c d e f g h i k) 3)
;;;; ( (A B C) (D E F G H I K))
(in-package :99)

(defun p17-split (lst n)
  (when (not (plusp n))
    (error "n must be positive"))
  (labels ((recur (ll n acc)
	     (cond
	       ((null ll) (reverse acc))
	       ((minusp n) (recur (cdr ll) n (cons (car ll) acc)))
	       ((zerop n) (cons (reverse acc) (list (recur ll (1- n) nil))))
	       (t (recur (cdr ll) (1- n) (cons (car ll) acc))))))
	   (recur lst n nil)))
  
