;;; (**) Node degree and graph coloration
;;;
;;; a) Write a function (degree graph node) that determines the degree
;;;    of a given node.
;;; b) Write a function that generates a list of all nodes of a graph
;;;    sorted according to decreasing degree.
;;; c) Use Welsh-Powell's algorithm to paint the nodes of a graph in
;;;    such a way that adjacent nodes have different colors.
(in-package :99-problems)

;;; Part A was completed in p85.lisp.

(defun degree-sorted-nodes (graph)
  "Return a list of the nodes of GRAPH sorted by decreasing degree."
  ;; Apply Schwartzian transform to the zip of vertices and degree
  ;; list. Stable-sort is not technically required, but it makes
  ;; testing easier.
  (mapcar #'first
	  (stable-sort (zip (vertices graph) (degrees graph)) #'> :key #'second)))

(defun vertex-coloring (graph &aux (adjacency-list (adjacency graph)))
  "Use the Welsh-Powell algorithm to color the vertices of GRAPH.

The input GRAPH should be an undirected-graph. The output will be an
  alist of (vertex . color) pairs."
  (assert (typep graph 'undirected-graph))
  ;; The Welsh-Powell algorithm proceeds as follows:
  ;; 1. All vertices are sorted according to decreasing valence. Call
  ;;    this list V.
  ;; 2. The first non-colored vertex v in V is colored with the first
  ;;    available color. Available means a color that was not
  ;;    previously used by the algorithm.
  ;; 3. The remaining part of the ordered list V is traversed and the
  ;;    same color is allocated to every vertex for which no adjacent
  ;;    vertex has the same color.
  ;; 4. Steps 2 and 3 are applied iteratively until all the vertices
  ;;    have been colored.
  (labels ((next-color (visited remaining color)
	     ;; Step 2
	     (if (null remaining)
		 visited
		 (same-color (acons (car remaining) color visited)
			     (cdr remaining)
			     color)))
	   (same-color (visited others color &aux remaining)
	     ;; Step 3
	     (if (null others)
		 visited
		 (progn (dolist (v others)
			  (if (adjacent-to-color-p v visited color)
			      (push v remaining)
			      (setf visited (acons v color visited))))
			(next-color visited (nreverse remaining) (1+ color)))))
	   (adjacent-to-color-p (vertex visited color)
	     (some (lambda (v) (eql color (cdr (assoc v visited))))
		   (adjacent-nodes adjacency-list vertex))))
    (next-color '() (degree-sorted-nodes graph) 0)))

(defun alist-group-by-values (alist)
  "Return a list of the keys of ALIST grouped according to their values."
  (let ((grouped (make-hash-table)))
    (dolist (pair alist)
      (symbol-macrolet ((place (gethash (cdr pair) grouped)))
	(setf place (cons (car pair) place))))
    (loop for value being the hash-values of grouped collect value)))

(defun adjacent-p (v1 v2 graph)
  "Return true if V1 and V2 are adjacent in GRAPH."
  (let ((adj-list (adjacency graph)))
    (or (member v1 (adjacent-nodes adj-list v2))
	(member v2 (adjacent-nodes adj-list v1)))))

(defun coloring-kosher-p (graph coloring)
  "Return t if COLORING is a proper vertex coloring of GRAPH."
  (every (lambda (same-colored-vertices)
	   (notany (lambda (pair)
		     (adjacent-p (first pair) (second pair) graph))
		  (combinations same-colored-vertices :length 2)))
	 (alist-group-by-values coloring)))

(define-test vertex-coloring-test
  (let* ((graph (mk-graph '(a b c d e f)
			  '((a b) (a d) (b c) (c f) (d e) (e f))))
	 (coloring (vertex-coloring graph)))
    (assert (coloring-kosher-p graph coloring))
    (assert-equality (lambda (a b) (set-equal a b :test #'equal))
		     '((a . 0) (c . 0) (e . 0) (b . 1) (d . 1) (f . 1))
		     coloring))
  (let* ((graph (mk-graph '(a b c d e f g h i j k)
			  '((a b) (a h) (b d) (c d) (d i)
			    (d k) (e k) (e f) (f g) (g h)
			    (h i) (h j) (h k) (i j) (j k))))
	 (coloring (vertex-coloring graph)))
    (assert (coloring-kosher-p graph coloring))
    (assert-equality (lambda (a b) (set-equal a b :test #'equal))
		     '((h . 0) (d . 0) (e . 0)
		       (k . 1) (i . 1) (a . 1) (f . 1) (c . 1)
		       (g . 2) (j . 2) (b . 2))
		     coloring)))
