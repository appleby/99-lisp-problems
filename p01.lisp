;;;; (*) Find the last box of a list.
;;;; Example:
;;;; * (my-last '(a b c d))
;;;; (D)

;;; Cheating?
;;; (defvar p01-last)
;;; (setf (symbol-function 'p01-last) #'last)

(defun p01-last (lst)
  (if (cdr lst)
      (p01-last (cdr lst))
      lst))

;;; Solution:
;;; http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/p01.lisp

;;; I think my solution is more elegant, though the explicit null test
;;; has me wondering whether the semantics of (if (cdr x) y z) and
;;; (if (null (cdr x)) z y) differ.

;;; After checking the hyperspec, it seems #'not and #'null are
;;; identical, only #'not is meant for boolean operands and #'null for
;;; testing the empty list.

(defun my_last (lista)
  (if (null lista)
      nil
    (if (null (rest lista))
	lista			 ; testa se a lista so tem um elemento
      (my_last (rest lista))		; recursao no resto da lista
      )
    )
  )
