;;;; (*) Count the leaves of a binary tree
;;;;
;;;; A leaf is a node with no successors. Write a function
;;;; count-leaves to count them.
;;;;
;;;; (count-leaves tree) returns the number of leaves of binary tree
;;;; tree
(in-package :99-problems)

(defun count-leaves (tree)
  (cond ((tree-empty-p tree) 0)
	((leaf-node-p tree) 1)
	(t (+ (count-leaves (tree-left tree))
	      (count-leaves (tree-right tree))))))

(define-test count-leaves-test
  (let ((inputs `((3 ,*t1*)
		  (1 ,*t2*)
		  (0 ,*t3*))))
    (loop for (expected tree) in inputs
       do (assert-eql expected (count-leaves tree)))))
