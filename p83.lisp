;;; (**) Construct all spanning trees
;;;
;;; Write a function (s-tree graph) to construct (by backtracking) all
;;; spanning trees of a given graph. With this function, find out how
;;; many spanning trees there are for the graph depicted to the
;;; left. The data of this example graph can be found in the file
;;; p83.dat. When you have a correct solution for the s-tree function,
;;; use it to define two other useful functions: (is-tree graph) and
;;; (is-connected graph). Both are five-minutes tasks!
(in-package :99)

(defvar *p83-graph* (mk-graph '(a b c d e f g h)
			      '((a b) (a d) (b c) (b e) (c e) (d e)
				(d f) (d g) (e h) (f g) (g h))))

(defun s-tree (graph)
  "Return the set of all spanning trees of GRAPH.

Each spanning tree is represented as an object of type
'undirected-graph.

This is procedure S from:
  Harold N. Gabow, Eugene W. Myers
  Finding All Spanning Trees of Directed and Undirected Graphs.
"
  ;; This is a direct translation of the pseudocode from the paper,
  ;; and makes for pretty hideous procedural lisp.
  (let* ((solutions '())

	 ;; The Gabow / Myers algorithm requires converting to a
	 ;; directed graph at the start.
	 (digraph (convert-to 'directed graph))

	 ;; The partial spanning tree constructed so far. Initialize
	 ;; it with any vertex from the graph.
	 (pst (mk-digraph (take 1 (vertices digraph)) '()))

	 ;; A list of all the edges from a vertex in pst to a vertex
	 ;; not in pst.
	 (f (let ((v (first (vertices pst))))
	      (remove-if-not (lambda (e) (eq v (first e)))
			     (edges digraph))))

	 ;; The last spanning tree we found. Used for bridge
	 ;; detection.
	 (L nil))
    (labels ((update-f (v)
               ;; Remove all edges (u, v) | u in pst.
	       (setf f (remove-if (lambda (e)
				    (and (eq v (second e))
					 (contains-vertex (first e) pst)))
				  f))

	       ;; Push all edges (v, w) | w not in pst.
	       (loop for e in (edges digraph)
		  if (and (eq v (first e)) (not (contains-vertex (second e) pst)))
		  do (push e f)))

	     (branch-p (v)
	       "Return t if v is the terminus of a bridge in digraph."
	       (loop for (w x) in (edges digraph)
		  never (and (eq x v) (not (path (adjacency L) v w)))))

	     (grow ()
	       (if (vertices-equal digraph pst)
		   (progn
		     (setf L pst)
		     (push (convert-to 'undirected pst) solutions))
		   (loop
		      with f-bar = nil
		      while (not (null f))
		      for e = (pop f)
		      for v = (second e)
		      for saved-f = (copy-list f)
		      do
			(progn
			  (setf pst (add-edge e pst))
			  (update-f v)
			  (grow)
			  (setf f saved-f)
			  (setf pst (remove-edge e pst))
			  (setf digraph (remove-edge e digraph))
			  (push e f-bar))
		      until (branch-p v)
		      finally
			(loop for e in f-bar
			   do (push e f)
			   do (setf digraph (add-edge e digraph)))))))
      (grow))
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
