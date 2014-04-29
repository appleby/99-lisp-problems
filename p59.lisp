;;;; (**) Construct height-balanced binary trees
;;;;
;;;; In a height-balanced binary tree, the following property holds
;;;; for every node: The height of its left subtree and the height of
;;;; its right subtree are almost equal, which means their difference
;;;; is not greater than one.
;;;;
;;;; Write a function hbal-tree to construct height-balanced binary
;;;; trees for a given height. The function should generate all
;;;; solutions. Put the letter 'x' as information into all nodes of
;;;; the tree.
;;;;
;;;; Example:
;;;; * (hbal-tree 3)
;;;; (X (X (X NIL NIL) (X NIL NIL)) (X (X NIL NIL) (X NIL NIL)))
;;;; = (X (X (X NIL NIL) (X NIL NIL)) (X (X NIL NIL) NIL))
;;;; etc......

(in-package :99)

(defun p59-hbal-tree (n)
  (cond ((<= n 0) '(()))
	((= n 1) '((x nil nil)))
	(t (let ((minus-1-trees (p59-hbal-tree (- n 1)))
		 (minus-2-trees (p59-hbal-tree (- n 2))))
	     (mapcar (lambda (x) (cons 'x x))
		     (append (cartesian-product minus-1-trees minus-1-trees)
			     (cartesian-product minus-1-trees minus-2-trees)
			     (cartesian-product minus-2-trees minus-1-trees)))))))

(defun p59-hbal-tree-print (n)
  (loop for tree in (p59-hbal-tree n) do (print tree)))
