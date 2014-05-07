;;;; (*) Count the nodes of a multiway tree
;;;;
;;;; Write a function nnodes which counts the nodes of a given
;;;; multiway tree.
;;;;
;;;; Example:
;;;; * (nnodes '(a f))
;;;; 2
(in-package :99)

(defun nnodes (tree)
  (ecase (type-of tree)
    ((null) 0)
    ((symbol) 1)
    ((cons) (reduce #'+ (mapcar #'nnodes tree)))))
