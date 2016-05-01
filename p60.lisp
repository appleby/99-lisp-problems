;;;; (**) Construct height-balanced binary trees with a given number
;;;; of nodes
;;;;
;;;; Consider a height-balanced binary tree of height H. What is the
;;;; maximum number of nodes it can contain?  Clearly, MAXN = 2**H -
;;;; 1. However, what is the minimum number MINN? This question is
;;;; more difficult. Try to find a recursive statement and turn it
;;;; into a function minnodes defined as follows:
;;;;
;;;; (min-nodes H) returns the minimum number of nodes in a
;;;; height-balanced binary tree of height H.
;;;;
;;;; On the other hand, we might ask: what is the maximum height H a
;;;; height-balanced binary tree with N nodes can have?
;;;;
;;;; (max-height N) returns the maximum height of a height-balanced
;;;; binary tree with N nodes
;;;;
;;;; Now, we can attack the main problem: construct all the
;;;; height-balanced binary trees with a given number of nodes.
;;;;
;;;; (hbal-tree-nodes N) returns all height-balanced binary trees with
;;;; N nodes.
;;;;
;;;; Find out how many height-balanced trees exist for N = 15.
(in-package :99-problems)

(defun min-nodes (height)
  (cond ((<= height 0) 0)
	((= height 1) 1)
	;; We could also define this recursively, viz.
	;; (+ (min-nodes (1- height)) 2)
	;; But you'd need an extra cond clause above:
	;; ((= height 2) 2)
	(t (- (* height 2) 2))))

(defun max-height (n)
  (cond ((<= n 0) 0)
	(t (values (truncate (/ (+ n 2) 2))))))

(defun balanced-partitions (n)
  (flet ((balanced (low high)
	   (<= (- (max-height high) (max-height low)) 1)))
    (loop
       for low from (truncate n 2) downto 0
       for high from (if (evenp n) low (1+ low)) upto n
       while (balanced low high)
       collect (list low high))))

(defun hbal-tree-nodes (n)
  (if (<= n 0)
      (list (make-empty-tree))
      (loop for (low high) in (balanced-partitions (1- n))
	 append (extend-trees
		 (generate-subtrees #'hbal-tree-nodes low high)))))

(defun hbal-tree-nodes-print (n)
  (loop for tree in (hbal-tree-nodes n) do (print tree)))

(define-test hbal-tree-nodes-test
  (let ((inputs '((0 (nil))
		  (1 ((x nil nil)))
		  (2 ((X NIL (X NIL NIL)) 
		      (X (X NIL NIL) NIL)))
		  (3 ((X (X NIL NIL) (X NIL NIL))))
		  (4 ((X (X NIL NIL) (X NIL (X NIL NIL))) 
		      (X (X NIL NIL) (X (X NIL NIL) NIL)) 
		      (X (X NIL (X NIL NIL)) (X NIL NIL)) 
		      (X (X (X NIL NIL) NIL) (X NIL NIL))))
		  (5 ((X (X NIL (X NIL NIL)) (X NIL (X NIL NIL))) 
		      (X (X NIL (X NIL NIL)) (X (X NIL NIL) NIL)) 
		      (X (X (X NIL NIL) NIL) (X NIL (X NIL NIL))) 
		      (X (X (X NIL NIL) NIL) (X (X NIL NIL) NIL)) 
		      (X (X NIL NIL) (X (X NIL NIL) (X NIL NIL))) 
		      (X (X (X NIL NIL) (X NIL NIL)) (X NIL NIL))))
		  (6 ((X (X NIL (X NIL NIL)) (X (X NIL NIL) (X NIL NIL))) 
		      (X (X (X NIL NIL) NIL) (X (X NIL NIL) (X NIL NIL))) 
		      (X (X (X NIL NIL) (X NIL NIL)) (X NIL (X NIL NIL))) 
		      (X (X (X NIL NIL) (X NIL NIL)) (X (X NIL NIL) NIL)))))))
    (loop for (n expected) in inputs
       do (assert-true (every (lambda (x) (member x expected :test #'tree-equal))
			      (hbal-tree-nodes n))))))
