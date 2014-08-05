;;;; (**) Truth tables for logical expressions (3).
;;;;
;;;; Generalize problem P47 in such a way that the logical expression
;;;; may contain any number of logical variables. Define table in a
;;;; way that (table List Expr) prints the truth table for the
;;;; expression Expr, which contains the logical variables enumerated
;;;; in List.
;;;;
;;;; Example:
;;;; * (table '(A B C) '((A and (B or C)) equ ((A and B) or (A and C)))).
;;;; true true true true
;;;; true true nil true
;;;; true nil true true
;;;; true nil nil true
;;;; nil true true true
;;;; nil true nil true
;;;; nil nil true true
;;;; nil nil nil true
(in-package :99)

(defun n-tuples (n values)
  (if (= n 0)
      '(())
      (loop for value in values
	 append (loop for tuple in (n-tuples (1- n) values)
		   collect (cons value tuple)))))

(defun make-env (vars values)
  (pairlis vars values))

(defun get-env (var env)
  (let ((pair (assoc var env)))
    (if pair
	(cdr pair)
	(error "Unbound variable ~a" var))))
	
(defun generate-all-possible-bindings (vars &optional (values '(t nil)))
  (loop for value-tuple in (n-tuples (length vars) values)
       collect (make-env vars value-tuple)))

(defparameter *valid-boolean-ops* '(and or nand nor xor impl equ))

(defun eval-bool (expr env)
  (cond ((symbolp expr) (get-env expr env))
	((consp expr)
	 (destructuring-bind (e1 op e2) expr
	   (if (member op *valid-boolean-ops*)
	       (eval (list op (eval-bool e1 env) (eval-bool e2 env)))
	       (error "Invalid boolean operation '~a' in expression: ~a" op expr))))
	(t (error "Invalid expression: ~a" expr))))

(defun table (vars expr)
  (flet ((print-column (value) (format t "~:[F~;T~] " value)))
    (loop for env in (generate-all-possible-bindings vars)
       do (loop for v in vars do (print-column (get-env v env)))
       do (print-column (eval-bool expr env))
       do (terpri))))
