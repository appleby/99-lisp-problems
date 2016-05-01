;;;; (**) Truth tables for logical expressions.
;;;;
;;;; Define functions and, or, nand, nor, xor, impl and equ (for
;;;; logical equivalence) which return the result of the respective
;;;; operation on boolean values.
;;;;
;;;; A logical expression in two variables can then be written in
;;;; prefix notation, as in the following example: (and (or A B) (nand
;;;; A B)).
;;;;
;;;; Write a function table which prints the truth table of a given
;;;; logical expression in two variables.
;;;;
;;;; Example:
;;;; * (table 'A 'B '(and A (or A B))).
;;;; true true true
;;;; true nil true
;;;; nil true nil
;;;; nil nil nil
(in-package :99-problems)

(defmacro nand (&rest x)
  `(not (and ,@x)))

(defmacro nor (&rest x)
  `(not (or ,@x)))

(defun xor (a b)
  (or (and a (not b)) (and (not a) b)))

(defun impl (a b)
  (or a (not b)))

(defun equ (a b)
  (not (xor a b)))

(defun table (sym1 sym2 expr)
  (loop for a in '(t nil)
     do (loop for b in '(t nil)
	   for bound-expr = (subst a sym1 (subst b sym2 expr))
	   do (format t "~:[F~;T~] ~:[F~;T~] ~:[F~;T~]~%" a b (eval bound-expr)))))

(define-test table-test
  (assert-prints
   "T T T
T F T
F T F
F F F"
   (table 'A 'B '(and A (or A B)))))
