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
(in-package :99)

(defun complete-binary-tree (n &optional (k 1))
  (if (= n 0)
      *the-empty-tree*
      (let* (
	     ;; h is the height of the largest perfect tree that is a
	     ;; subtree of this tree. If the tree we are constructing
	     ;; is a perfect tree, then the h is the height of this
	     ;; tree; otherwise, the height of this tree is h+1.
	     (h (floor (log (1+ n) 2)))

	     ;; i is the total number of nodes in a perfect subtree of
	     ;; height h.
	     (i (1- (expt 2 h)))

	     ;; r is the maximum number of nodes that would fit in
	     ;; level h+1 of a perfect tree.
	     (r (expt 2 h))

	     ;; m is the actual number of nodes in level h+1 of this
	     ;; tree. Note that if this is a perfect tree, then m = 0.
	     (m (- n i))

	     ;; l is the total number of nodes in the left subtree.
	     (l (+ (/ (1- i) 2) (min m (/ r 2)))))
	(list k
	      (complete-binary-tree l (* 2 k))
	      (complete-binary-tree (- n 1 l) (+ (* 2 k) 1))))))

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
