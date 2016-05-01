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

(in-package :99-problems)

(defun hbal-tree (n)
  (cond ((<= n 0) (list (make-empty-tree)))
	((= n 1) (list (make-leaf-node)))
	(t (extend-trees
	    (append (generate-subtrees #'hbal-tree (- n 1) (- n 1))
		    (generate-subtrees #'hbal-tree (- n 1) (- n 2)))))))

(defun hbal-tree-print (n)
  (loop for tree in (hbal-tree n) do (print tree)))

(define-test hbal-tree-test
  (let ((inputs '((0 (NIL))
		  (1 ((X NIL NIL)))
		  (2 ((X (X NIL NIL) (X NIL NIL))
		      (X (X NIL NIL) NIL)
		      (X NIL (X NIL NIL))))
		  (3 ((X (X (X NIL NIL) (X NIL NIL)) (X (X NIL NIL) (X NIL NIL)))
		      (X (X (X NIL NIL) (X NIL NIL)) (X (X NIL NIL) NIL))
		      (X (X (X NIL NIL) (X NIL NIL)) (X NIL (X NIL NIL)))
		      (X (X (X NIL NIL) NIL) (X (X NIL NIL) (X NIL NIL)))
		      (X (X (X NIL NIL) NIL) (X (X NIL NIL) NIL))
		      (X (X (X NIL NIL) NIL) (X NIL (X NIL NIL)))
		      (X (X NIL (X NIL NIL)) (X (X NIL NIL) (X NIL NIL)))
		      (X (X NIL (X NIL NIL)) (X (X NIL NIL) NIL))
		      (X (X NIL (X NIL NIL)) (X NIL (X NIL NIL)))
		      (X (X (X NIL NIL) (X NIL NIL)) (X NIL NIL))
		      (X (X (X NIL NIL) NIL) (X NIL NIL))
		      (X (X NIL (X NIL NIL)) (X NIL NIL))
		      (X (X NIL NIL) (X (X NIL NIL) (X NIL NIL)))
		      (X (X NIL NIL) (X (X NIL NIL) NIL))
		      (X (X NIL NIL) (X NIL (X NIL NIL))))))))
    (loop for (n expected) in inputs
       do (assert-true (tree-solutions-valid expected (hbal-tree n))))))
