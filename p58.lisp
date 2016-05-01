;;;;  (**) Generate-and-test paradigm
;;;;
;;;; Apply the generate-and-test paradigm to construct all symmetric,
;;;; completely balanced binary trees with a given number of
;;;; nodes. Example:
;;;;
;;;; * (sym-cbal-trees-print 5)
;;;; (X (X NIL (X NIL NIL)) (X (X NIL NIL) NIL))
;;;; (X (X (X NIL NIL) NIL) (X NIL (X NIL NIL)))
;;;; ...
;;;;
;;;; How many such trees are there with 57 nodes? Investigate about
;;;; how many solutions there are for a given number of nodes. What if
;;;; the number is even? Write an appropriate function.

(in-package :99-problems)

(defun sym-cbal-trees (n)
  (loop for tree in (cbal-tree n)
     when (symmetric tree)
     collect tree))

(define-test sym-cbal-trees-test
  (assert-equal
   '((X (X NIL (X NIL NIL)) (X (X NIL NIL) NIL))
     (X (X (X NIL NIL) NIL) (X NIL (X NIL NIL))))
   (sym-cbal-trees 5)))
