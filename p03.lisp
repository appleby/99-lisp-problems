;;;; (*) Find the K'th element of a list.
;;;; The first element in the list is number 1.
;;;; Example:
;;;; * (element-at '(a b c d e) 3)
;;;; C

(defun p03-element-at (lst n)
  (nth (1- n) lst))
