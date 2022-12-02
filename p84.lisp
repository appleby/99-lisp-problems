;;;; (**) Construct a minimum spanning tree
;;;;
;;;; Write a function (ms-tree graph) to construct a minimum spanning
;;;; tree of a given labelled graph. The function must also return the
;;;; minimum weight. Hint: Use the algorithm of Prim. A small
;;;; modification of the solution of P83 does the trick. The data of
;;;; the example graph to the right can be found in the file p84.dat.
(in-package :99-problems)

(defvar *p84-graph*
  ;;; Defines the following graph
  ;;;
  ;;; (a)--5--(b)--2-(c)
  ;;;  |         \    |
  ;;;  3          4   6
  ;;;  |            \ |
  ;;; (d)------7-----(e)
  ;;;  | \            |
  ;;;  4  3           5
  ;;;  |   \          |
  ;;; (f)-4-(g)---1--(h)
  ;;;
  (mk-labeled-graph '(a b c d e f g h)
		    '((a b 5) (a d 3) (b c 2) (b e 4) (c e 6) (d e 7) (d f 4)
		      (d g 3) (e h 5) (f g 4) (g h 1))))

(defun ms-tree (graph)
  "Return a minimum spanning tree of GRAPH.

This is Prim's algorithm."
  (labels ((weight (edge)
	     (third edge))
	   (sort-edges (edges)
	     (sort edges #'< :key #'weight)))
    (loop
       with root = (first (vertices graph))
       with mst = (mk-labeled-graph (list root) '())
       with cut-set = (sort-edges (adjacent-edges root graph))
       until (or (vertices-equal mst graph) (null cut-set))
       for min-edge = (pop cut-set)
       sum (weight min-edge) into total-weight
       do (setf mst (add-edge min-edge mst))
       ;; Sorting is wasteful. It's tempting to use a heap for the
       ;; cut-set or just do sorted insertions, but that would
       ;; preclude reusing next-cut-set from the p83. Also, cl-heap's
       ;; api for deleting an item from the heap requires you to save
       ;; the item index returned by add-to-heap, which precludes
       ;; using add-all-to-heap, which is a bummer.
       do (setf cut-set
		(sort-edges (next-cut-set (second min-edge) cut-set mst graph)))
       finally (return (values total-weight mst)))))

(define-test ms-tree-test
  ;;; The four solutions.
  ;;;
  ;;; (a)--5--(b)--2-(c)       (a)--5--(b)--2-(c)
  ;;;  |         \              |         \
  ;;;  3          4     	3          4
  ;;;  |            \           |            \
  ;;; (d)	     (e)       (d)            (e)
  ;;;  | \	      	          \
  ;;;  4  3	      	           3
  ;;;  |   \	      	            \
  ;;; (f)   (g)---1--(h)       (f)-4-(g)---1--(h)
  ;;;
  ;;; (a)     (b)--2-(c)       (a)     (b)--2-(c)
  ;;;  |         \      	|         \
  ;;;  3          4     	3          4
  ;;;  |            \   	|            \
  ;;; (d)            (e)       (d)            (e)
  ;;;    \            | 	| \            |
  ;;;     3           5         4  3           5
  ;;;      \          | 	|   \          |
  ;;; (f)-4-(g)---1--(h)       (f)   (g)---1--(h)
  ;;;
  (let* ((vertices '(a b c d e f g h))
	 (common-edges '((a d 3) (d g 3) (g h 1) (b e 4) (b c 2)))
	 (swappable-edges (cartesian-product '((d f 4) (f g 4))
					     '((a b 5) (e h 5))))
	 (solutions (loop for es in swappable-edges
		       collect (mk-labeled-graph vertices
						 (append common-edges es)))))
    (multiple-value-bind (weight mst) (ms-tree *p84-graph*)
      (assert-eql 22 weight)
      (assert-true (some (lambda (solution) (graph-equal mst solution))
			 solutions)))))
