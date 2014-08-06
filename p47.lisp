;;;; (*) Truth tables for logical expressions (2).
;;;;
;;;; Modify problem P46 by accepting expressions written in infix
;;;; notation, with all parenthesis present. This allows us to write
;;;; logical expression in a more natural way, as in the example: (A
;;;; and (A or (not B))).
;;;;
;;;; Example:
;;;; * (table 'A 'B '(A and (A or (not B)))).
;;;; true true true
;;;; true nil true
;;;; nil true nil
;;;; nil nil nil
(in-package :99)

(defun bind-vars (expr &rest vars-and-values)
  (loop
     with bound-expr = expr
     for (var value) on vars-and-values by #'cddr
     do (setf bound-expr (subst value var bound-expr))
     finally (return bound-expr)))

(defun infix->prefix (expr)
  (let ((boolean-ops '(and or nand nor xor equ impl)))
    (cond ((or (atom expr) (eq (car expr) 'not)) expr)
	  (t (destructuring-bind (a op b) expr
	       (if (member op boolean-ops)
		   (list op (infix->prefix a) (infix->prefix b))
		   (error "Invalid boolean operator '~a' in expression: ~a"
			  op expr)))))))

;;; One could also solve this by writing a mini-evaluator for boolean
;;; expressions. I wanted to try transforming the infix expressions to
;;; prefix and eval'ing the result. The next problem is to generalize
;;; this to N free variables, so I'll write an evaluator for that one.
(defun table-infix (sym1 sym2 infix-expr)
  (loop for a in '(t nil) do
       (loop for b in '(t nil)
	     for expr = (infix->prefix (bind-vars infix-expr sym1 a sym2 b))
	  do (format t "~:[F~;T~] ~:[F~;T~] ~:[F~;T~]~%" a b (eval expr)))))

(define-test table-infix-test
  (assert-prints
   "T T T
T F T
F T F
F F F"
   (table-infix 'A 'B '(A and (A or (not B))))))
