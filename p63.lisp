;;;; (**) Construct a complete binary tree

;;;; A complete binary tree with height H is defined as follows: The
;;;; levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e
;;;; 2**(i-1) at the level i, note that we start counting the levels
;;;; from 1 at the root). In level H, which may contain less than the
;;;; maximum possible number of nodes, all the nodes are
;;;; "left-adjusted". This means that in a levelorder tree traversal
;;;; all internal nodes come first, the leaves come second, and empty
;;;; successors (the nil's which are not really nodes!) come last.
;;;;
;;;; Particularly, complete binary trees are used as data structures
;;;; (or addressing schemes) for heaps.
;;;;
;;;; We can assign an address number to each node in a complete binary
;;;; tree by enumerating the nodes in levelorder, starting at the root
;;;; with number 1. In doing so, we realize that for every node X with
;;;; address A the following property holds: The address of X's left
;;;; and right successors are 2*A and 2*A+1, respectively, supposed
;;;; the successors do exist. This fact can be used to elegantly
;;;; construct a complete binary tree structure. Write a function
;;;; complete-binary-tree with the following specification:
;;;;
;;;; (complete-binary-tree N) returns a complete binary tree with N
;;;; nodes
;;;;
;;;; Test your function in an appropriate way.
(in-package :99-problems)

(defun complete-binary-tree (n &optional (k 1))
  (if (> k n)
      *the-empty-tree*
      (list k
	    (complete-binary-tree n (* 2 k))
	    (complete-binary-tree n (+ (* 2 k) 1)))))

(define-test complete-binary-tree-test
  (let ((inputs '((0  NIL)
		  (1 (1 NIL NIL))
		  (2 (1 (2 NIL NIL) NIL))
		  (3 (1 (2 NIL NIL) (3 NIL NIL)))
		  (4 (1 (2 (4 NIL NIL) NIL) (3 NIL NIL)))
		  (5 (1 (2 (4 NIL NIL) (5 NIL NIL)) (3 NIL NIL)))
		  (6 (1 (2 (4 NIL NIL) (5 NIL NIL)) (3 (6 NIL NIL) NIL)))
		  (7 (1 (2 (4 NIL NIL) (5 NIL NIL)) (3 (6 NIL NIL) (7 NIL NIL))))
		  (8 (1 (2 (4 (8 NIL NIL) NIL) (5 NIL NIL)) (3 (6 NIL NIL) (7 NIL NIL))))
		  (15 (1 (2 (4 (8 NIL NIL) (9 NIL NIL)) (5 (10 NIL NIL) (11 NIL NIL)))
		       (3 (6 (12 NIL NIL) (13 NIL NIL)) (7 (14 NIL NIL) (15 NIL NIL))))))))
    (loop for (i expected) in inputs
	 do (assert-true (tree-equal expected (complete-binary-tree i))))))
