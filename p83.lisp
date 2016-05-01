;;;; (**) Construct all spanning trees
;;;;
;;;; Write a function (s-tree graph) to construct (by backtracking) all
;;;; spanning trees of a given graph. With this function, find out how
;;;; many spanning trees there are for the graph depicted to the
;;;; left. The data of this example graph can be found in the file
;;;; p83.dat. When you have a correct solution for the s-tree function,
;;;; use it to define two other useful functions: (is-tree graph) and
;;;; (is-connected graph). Both are five-minutes tasks!
(in-package :99-problems)

(defvar *p83-graph* (mk-graph '(a b c d e f g h)
			      '((a b) (a d) (b c) (b e) (c e) (d e)
				(d f) (d g) (e h) (f g) (g h))))

(defun next-cut-set (v cut-set pst graph)
  ;; Generate a new list of edges for the next invocation
  ;; of grow. We have just selected the edge e=(u,v). We
  ;; now need to:
  ;;
  ;; 1) Add all edges (v,w) where w is not in pst. In
  ;;    other words, add all outbound edges from the new
  ;;    vertex v that we just selected, but skip any that
  ;;    introduce a cycle in pst.
  ;;
  ;; 2) Remove all edges (x,v) where x is in pst. That
  ;;    is, any existing edges that point to our new
  ;;    vertex should be removed, again to prevent
  ;;    cycles.
  (append
   ;; Add all edges (v,w) where w is not in pst.
   (remove-if (lambda (e) (contains-vertex (second e) pst))
	      (adjacent-edges v graph))
   ;; Remove all edges (x,v) where x is in pst.
   (remove-if (lambda (e) (eq v (second e)))
	      cut-set)))

(defun s-tree (graph)
  "Return the set of all spanning trees of GRAPH.

Each spanning tree is represented as an object of type
`undirected-graph'.

This is procedure S from:
  Harold N. Gabow, Eugene W. Myers
  Finding All Spanning Trees of Directed and Undirected Graphs.
"
  (let ((solutions '())
	;; The last spanning tree we found. Used for bridge
	;; detection.
	last-stree)
    (labels ((branch-p (v digraph)
	       "Return t if v is the terminus of a bridge in digraph."
	       (loop for (w x) in (edges digraph)
		  never (and (eq x v) (not (path (adjacency last-stree) v w)))))

	     (grow (cut-set pst digraph)
	       (if (vertices-equal digraph pst)
		   (progn
		     (setf last-stree pst)
		     (push (convert-to 'undirected pst) solutions))
		   (loop
		      for (e . es) on cut-set
		      for v = (second e)
		      do
			(progn
			  (grow (next-cut-set v es pst digraph)
				(add-edge e pst)
				digraph)
			  (setf digraph (remove-edge e digraph)))
		      until (branch-p v digraph)))))
      (let* ( ;; The Gabow / Myers algorithm requires a directed graph
	     ;; as input. Spanning trees are converted back to
	     ;; undirected graphs before being pushed onto SOLUTIONS.
	     (digraph (convert-to 'directed graph))

	     ;; Choose a vertex from digraph as the root. If GRAPH was
	     ;; undirected, then the choice of root is arbitrary.
	     (root (first (vertices digraph)))

	     ;; The partial spanning tree constructed so far.
	     (pst (mk-digraph (list root) '()))

	     ;; A list of all the edges from a vertex in pst to a vertex
	     ;; not in pst.
	     (cut-set (adjacent-edges root digraph)))
	(grow cut-set pst digraph)))
    solutions))

(defun is-connected (graph)
  (not (null (s-tree graph))))

(defun is-tree (graph)
  (let ((spanning-trees (s-tree graph)))
    (and (= 1 (length spanning-trees))
	 (graph-equal graph (first spanning-trees)))))

(define-test is-connected-test
  (assert-true (is-connected *p83-graph*))
  (assert-true (is-connected (mk-graph '(a b c) '((a b) (b c)))))
  (assert-false (is-connected (mk-graph '(a b c) '((a b))))))

(define-test is-tree-test
  (assert-true (is-tree (mk-graph '(a) '())))
  (assert-true (is-tree (mk-graph '(a b) '((a b)))))
  (assert-true (is-tree (mk-graph '(a b c) '((a b) (a c)))))
  (assert-false (is-tree (mk-graph '(a b c) '((a b) (a c) (b c)))))
  (assert-false (is-tree *p83-graph*)))

(define-test s-tree-test
  (flet ((graph-set-equal (x y)
	   (set-equal x y :test #'graph-equal)))
    (assert-eq 112 (length (s-tree *p83-graph*)))
    (assert-equality #'graph-set-equal
		     `(,(mk-graph '(a) '()))
		     (s-tree (mk-graph '(a) '())))
    (assert-equality #'graph-set-equal
		     `(,(mk-graph '(a b) '((a b))))
		     (s-tree (mk-graph '(a b) '((a b)))))
    (assert-equality #'graph-set-equal
		     `(,(mk-graph '(a b c) '((a b) (b c)))
		       ,(mk-graph '(a b c) '((a c) (c b)))
		       ,(mk-graph '(a b c) '((a b) (a c))))
		     (s-tree (mk-graph '(a b c) '((a b) (a c) (b c)))))
    (assert-equality #'graph-set-equal
		     `(,(mk-graph '(a b c d e) '((a b) (b c) (c e) (d e)))
		       ,(mk-graph '(a b c d e) '((a b) (b c) (b e) (d e)))
		       ,(mk-graph '(a b c d e) '((a b) (a d) (b c) (b e)))
		       ,(mk-graph '(a b c d e) '((a b) (a d) (b c) (c e)))
		       ,(mk-graph '(a b c d e) '((a b) (b e) (c e) (d e)))
		       ,(mk-graph '(a b c d e) '((a b) (a d) (b e) (c e)))
		       ,(mk-graph '(a b c d e) '((a b) (a d) (c e) (d e)))
		       ,(mk-graph '(a b c d e) '((a b) (a d) (b c) (d e)))
		       ,(mk-graph '(a b c d e) '((a d) (b c) (c e) (d e)))
		       ,(mk-graph '(a b c d e) '((a d) (b e) (c e) (d e)))
		       ,(mk-graph '(a b c d e) '((a d) (b c) (b e) (d e))))
		     (s-tree (mk-graph '(a b c d e)
				       '((a b) (a d) (b c) (b e) (c e) (d e)))))))
