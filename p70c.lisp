;;;; (*) Count the nodes of a multiway tree
;;;;
;;;; Write a function nnodes which counts the nodes of a given
;;;; multiway tree.
;;;;
;;;; Example:
;;;; * (nnodes '(a f))
;;;; 2
(in-package :99-problems)

(defun nnodes (tree)
  (ecase (type-of tree)
    ((null) 0)
    ((symbol) 1)
    ((cons) (reduce #'+ (mapcar #'nnodes tree)))))

(define-test nnodes-test
  (assert-eq 0 (nnodes '()))
  (assert-eq 1 (nnodes 'a))
  (assert-eq 2 (nnodes '(a b)))
  (assert-eq 3 (nnodes '(a b c)))
  (assert-eq 3 (nnodes '(a (b c))))
  (assert-eq 4 (nnodes '(a (b c) d))))
