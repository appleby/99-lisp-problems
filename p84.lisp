;;;; (**) Construct a minimum spanning tree
;;;;
;;;; Write a function (ms-tree graph) to construct a minimum spanning
;;;; tree of a given labelled graph. The function must also return the
;;;; minimum weight. Hint: Use the algorithm of Prim. A small
;;;; modification of the solution of P83 does the trick. The data of
;;;; the example graph to the right can be found in the file p84.dat.
(in-package :99)

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
  (values 0 graph))

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
    (multiple-value-bind (min-weight mst) (ms-tree *p84-graph*)
      (assert-eq 22 min-weight)
      (assert-true (some (lambda (solution) (graph-equal mst solution))
			 solutions)))))
