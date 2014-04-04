;;;;  (**) Rotate a list N places to the left.

;;;; Examples:
;;;; * (rotate '(a b c d e f g h) 3)
;;;; (D E F G H A B C)
;;;; * (rotate '(a b c d e f g h) -2)
;;;; (G H A B C D E F)

;;;; Hint: Use the predefined functions length and append, as well as
;;;; the result of problem P17.

(load "p17.lisp")

(defun p19-rotate (lst n)
  (let ((split-point (mod n (length lst))))
    (if (zerop split-point)
	(copy-list lst)
	(destructuring-bind (head tail) (p17-split lst split-point)
	  (append tail head)))))
