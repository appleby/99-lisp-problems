;;;; (*) Collect the leaves of a binary tree in a list
;;;;
;;;; A leaf is a node with no successors. Write a function leaves to
;;;; return them in a list.
;;;;
;;;; (leaves tree) returns the list of all leaves of the binary tree
;;;; tree
(in-package :99)

(defun leaves (tree)
  (cond ((tree-empty-p tree) *the-empty-tree*)
	((leaf-node-p tree) (list tree))
	(t (append (leaves (tree-left tree))
		   (leaves (tree-right tree))))))

(define-test leaves-test
  (let ((inputs `((((d nil nil) (e nil nil) (g nil nil))
		   ,*t1*)
		  (((a nil nil))
		   ,*t2*)
		  (,*the-empty-tree* ,*t3*))))
    (loop for (expected tree) in inputs
       do (assert-true (tree-solutions-valid expected (leaves tree))))))
