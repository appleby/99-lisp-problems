;;;;  (**) Rotate a list N places to the left.

;;;; Examples:
;;;; * (rotate '(a b c d e f g h) 3)
;;;; (D E F G H A B C)
;;;; * (rotate '(a b c d e f g h) -2)
;;;; (G H A B C D E F)

;;;; Hint: Use the predefined functions length and append, as well as
;;;; the result of problem P17.

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

(defun p19-rotate (lst n)
  (let ((split-point (mod n (length lst))))
    (if (zerop split-point)
	(copy-list lst)
	(destructuring-bind (head tail) (p17-split lst split-point)
	  (append tail head)))))
