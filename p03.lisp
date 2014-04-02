;;;; (*) Find the K'th element of a list.
;;;; The first element in the list is number 1.
;;;; Example:
;;;; * (element-at '(a b c d e) 3)
;;;; C

(defun p03-element-at (lst n)
  (nth (1- n) lst))

;;; Solution
;;; http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/p03.lisp

;;; The first solution is bad style, imho. The second is close to what
;;; I had in mind before I decided to just use #'nth :).

;;; Função que retorna o elemento na K-ésima posição de ;;;
;;; uma lista. Se a posição desejada eh maior que a     ;;;
;;; última da lista, retorna NIL.                       ;;;

(defun element-at (org-list pos &optional (ini 1))
    (if (eql ini pos)
        (car org-list)
        (element-at (cdr org-list) pos (+ ini 1))))

;;; Outra solucao

(defun element-at (lista n)
  (if (= n 1)
      ;; o primeiro elemento esta na posicao 1
      (first lista)
    (element-at (rest lista) (1- n))
    )
  )
