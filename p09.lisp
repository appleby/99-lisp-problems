;;;; (**) Pack consecutive duplicates of list elements into sublists.
;;;; If a list contains repeated elements they should be placed in separate sublists.

;;;; Example:
;;;; * (pack '(a a a a b c c a a d e e e e))
;;;; ((A A A A) (B) (C C) (A A) (D) (E E E E))
(in-package :99)

(defun pack (lst)
  (labels ((recur (lst acc)
	     (let ((this (car lst))    ; relies on (car '()) ==> '()
		   (next (cadr lst)))  ; likewise (cadr '()) ==> '()
	       (cond
		 ((null lst) acc)
		 ((equalp this next)
		  (recur (cdr lst) (cons this acc)))
		 (t (cons (cons this acc) (recur (cdr lst) '())))))))
    (recur lst '())))
