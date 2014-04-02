;;;; (*) Find the last but one box of a list.
;;;; Example:
;;;; * (my-but-last '(a b c d))
;;;; (C D)

(defun p02-but-last (lst)
  (if (cddr lst)
      (p02-but-last (cdr lst))
      lst))

;;;; Solution
;;;; http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/p02.lisp

;;;; This actually strikes me as a weird solution. If you're allowed
;;;; to use cl functions like #'length and #'reverse and aren't
;;;; expected to cdr down the list, then why not just use #'nthcdr?
;;;; It's also strange the example function above is named
;;;; my-but-last, but the behavoir is not the same as cl:butlast.

(defun penultimo (lista)
  (let ((reverso (reverse lista)))	; coloca na variavel reverso o
					; reverse de lista
    (cond
     ((null reverso) nil)
     ;; se a lista tem 2 ou menos elementos, retorno a propria
     ((<= (length reverso) 2)  lista)
     ;; se tiver mais de 2 elementos, construo uma lista
     (t (list (second reverso) (first reverso)))
     )
    )
  )
