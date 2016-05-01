;;;; (**) Layout a binary tree (1)
;;;;
;;;; Consider a binary tree as the usual symbolic expression (X L R)
;;;; or nil. As a preparation for drawing the tree, a layout algorithm
;;;; is required to determine the position of each node in a
;;;; rectangular grid. Several layout methods are conceivable, one of
;;;; them is shown in the illustration below.
;;;;
;;;; In this layout strategy, the position of a node v is obtained by
;;;; the following two rules:
;;;;
;;;; x(v) is equal to the position of the node v in the inorder
;;;; sequence y(v) is equal to the depth of the node v in the tree
;;;;
;;;; In order to store the position of the nodes, we extend the
;;;; symbolic expression representing a node (and its successors) as
;;;; follows:
;;;;
;;;; nil represents the empty tree (as usual). (W X Y L R) represents
;;;; a (non-empty) binary tree with root W "positioned" at (X,Y), and
;;;; subtrees L and R
;;;;
;;;; Write a function layout-binary-tree with the following
;;;; specification:
;;;;
;;;; (layout-binary-tree tree) returns the "positioned" binary tree
;;;; obtained from the binary tree tree
;;;;
;;;; Test your function in an appropriate way.
(in-package :99-problems)

(defun make-layout-node (elem x y left right)
  (list elem x y left right))

(defun make-layout-leaf (elem x y)
  (make-layout-node elem x y nil nil))

(defun layout-x (tree)
  (cadr tree))

(defun layout-binary-tree (tree)
  (labels ((layout (tree x y)
	     (if (tree-empty-p tree)
		 (values x *the-empty-tree*)
		 (multiple-value-bind (left-x left-tree)
		     (layout (tree-left tree) x (1+ y))
		   (multiple-value-bind (right-x right-tree)
		       (layout (tree-right tree) (1+ left-x) (1+ y))
		     (values right-x (make-layout-node
				      (tree-elem tree)
				      (1+ left-x) y
				      left-tree right-tree)))))))
    (nth-value 1 (layout tree 0 1))))

(define-test layout-binary-tree-test
  (let ((inputs `((,*t1* (A 4 1
			    (B 2 2 (D 1 3 NIL NIL) (E 3 3 NIL NIL))
			    (C 5 2 NIL (F 7 3 (G 6 4 NIL NIL) NIL))))
		  (,*t2* (A 1 1 NIL NIL))
		  (,*t3* NIL))))
    (loop for (input expected) in inputs
       do (assert-equality #'tree-equal expected (layout-binary-tree input)))))
