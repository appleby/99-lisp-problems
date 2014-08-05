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

(defun cartesian-product (list1 list2)
  (loop for x in list1 append
       (loop for y in list2 collect (list x y))))

(defun combine-subtrees (trees1 trees2)
  (append (cartesian-product trees1 trees2)
	  (cartesian-product trees2 trees1)))

(defun generate-subtrees (tree-fn left right)
  (let* ((left-trees (funcall tree-fn left)))
    (if (= left right)
	(cartesian-product left-trees left-trees)
	(combine-subtrees left-trees (funcall tree-fn right)))))

(defun extend-trees (trees &optional (symbol 'x))
  (mapcar (lambda (tree) (cons symbol tree)) trees))

;;; Should memoize.
(defun cbal-tree (n)
  (if (= n 0)
      (list *the-empty-tree*)
      (let* ((n1 (truncate (1- n) 2))
	     (n2 (- n 1 n1)))
	(extend-trees
	 (generate-subtrees #'cbal-tree n1 n2)))))

(defun tree-solutions-valid (expected solutions)
  (every (lambda (tree) (member tree expected :test #'tree-equal))
	 solutions))

(define-test cbal-tree
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
       do (assert-true (tree-solutions-valid expected (cbal-tree n))))))
