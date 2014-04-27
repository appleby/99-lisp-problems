;;;; (**) Construct completely balanced binary trees
;;;;
;;;; In a completely balanced binary tree, the following property
;;;; holds for every node: The number of nodes in its left subtree and
;;;; the number of nodes in its right subtree are almost equal, which
;;;; means their difference is not greater than one.
;;;;
;;;; Write a function cbal-tree to construct completely balanced
;;;; binary trees for a given number of nodes. The function should
;;;; generate all solutions. Put the symbol 'x' as information into
;;;; all nodes of the tree.
;;;;
;;;; Example:
;;;; * (cbal-tree-print 4)
;;;; (X (X NIL NIL) (X NIL (X NIL NIL)))
;;;; (X (X NIL NIL) (X (X NIL NIL) NIL))
;;;; etc......
;;;; Note: you can either print the trees or return a list with them all.
;;;;
;;;; * (cbal-tree 4)
;;;; ((X (X NIL NIL) (X NIL (X NIL NIL))) (X (X NIL NIL) (X (X NIL NIL) NIL)) ......)

(in-package :99)

(defun p55-unique-pairs (trees1 trees2)
  (flet ((pair-tree-equal (pair1 pair2)
	   (and (tree-equal (car pair1) (car pair2))
		(tree-equal (cdr pair1) (cdr pair2))))
	 (cartesian-product (list1 list2)
	   (loop for x in list1 append
		(loop for y in list2 collect (list x y)))))
    (union (cartesian-product trees1 trees2)
	   (cartesian-product trees2 trees1)
	   :test #'pair-tree-equal)))

;;; Should memoize.
(defun p55-cbal-tree (n)
  (if (= n 0)
      (list nil)
      (let* ((n1 (truncate (1- n) 2))
	     (n2 (- n 1 n1))
	     (subtree-1 (p55-cbal-tree n1)))
	(let ((left-right-tree-pairs
	       (if (= n1 n2)
		   (n-tuples 2 subtree-1)
		   (p55-unique-pairs subtree-1 (p55-cbal-tree n2)))))
	  (loop for (left-tree right-tree) in left-right-tree-pairs
	     collect (list 'x left-tree right-tree))))))


(define-test p55-cbal-tree
  (let ((inputs '((0 (NIL))
		  (1 ((X NIL NIL)))
		  (2 ((X NIL (X NIL NIL)) (X (X NIL NIL) NIL)))
		  (3 ((X (X NIL NIL) (X NIL NIL))))
		  (4 ((X (X NIL NIL) (X (X NIL NIL) NIL)) (X (X NIL NIL) (X NIL (X NIL NIL)))
		      (X (X NIL (X NIL NIL)) (X NIL NIL)) (X (X (X NIL NIL) NIL) (X NIL NIL))))
		  (5 ((X (X NIL (X NIL NIL)) (X NIL (X NIL NIL)))
		      (X (X NIL (X NIL NIL)) (X (X NIL NIL) NIL))
		      (X (X (X NIL NIL) NIL) (X NIL (X NIL NIL)))
		      (X (X (X NIL NIL) NIL) (X (X NIL NIL) NIL)))))))
    (loop for (n expected) in inputs
       do (assert-true (every #'tree-equal expected (p55-cbal-tree n))))))
