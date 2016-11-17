;;; (***) Von Koch's conjecture
;;;
;;; Several years ago I met a mathematician who was intrigued by a
;;; problem for which he didn't know a solution. His name was Von
;;; Koch, and I don't know whether the problem has been solved since.
;;;
;;; d  e--f     1     5-----4      i g  d--k     p
;;; |  |       6|    2|  1          \|  |        |
;;; a--b--c     7-----3-----6        a--c--e--q--n
;;; |          5|  4     3          /|  |     |
;;; g           2                  h b  f     m
;;;
;;; Anyway the puzzle goes like this: given a tree with N nodes (and
;;; hence N-1 edges), find a way to enumerate the nodes from 1 to N
;;; and, accordingly, the edges from 1 to N-1 in such a way that, for
;;; each edge K, the difference of its node numbers equals K. The
;;; conjecture is that this is always possible.
;;;
;;; For small trees the problem is easy to solve by hand. However, for
;;; larger trees, and 14 is already very large, it is extremely
;;; difficult to find a solution. And remember, we don't know for sure
;;; whether there is always a solution!
;;;
;;; Write a function that calculates a numbering scheme for a given
;;; tree. What is the solution for the larger tree pictured above?
;;;
;;; +--+ +--+      +--+     +--+              +--+
;;; |11| | 2|      | 3|==6==| 9|              |10|
;;; +--+ +--+      +--+     +--+              +--+
;;;   \   ||        ||                         ||
;;;  10\  ||1       ||11                       ||2
;;;     \ ||        ||                         ||
;;;      +--+      +--+     +--+     +--+     +--+
;;;      | 1|==13==|14|==9==| 5|==7==|12|==4==| 8|
;;;      +--+      +--+     +--+     +--+     +--+
;;;     / ||        ||                ||
;;;  12/  ||3       ||8               ||5
;;;   /   ||        ||                ||
;;; +--+ +--+      +--+              +--+
;;; |13| | 4|      | 6|              | 7|
;;; +--+ +--+      +--+              +--+
(in-package :99-problems)

(defun von-koch (graph)
  "Return a graceful labeling of GRAPH."
  ;; Slow and steady backtracking search. Does brute force search,
  ;; pruning dead ends. Makes no attempt to minimize consing. Takes
  ;; ~1s to solve the 14-node graph on my laptop.
  (assert (is-tree graph))
  (assert (every #'symbolp (vertices graph))) ; required by next-graph
  (let ((adj-list (adjacency graph)))
    (labels ((next-graph (vertex new-vertex labeled-graph)
	       ;; Return the new graph produced by renaming vertex to
	       ;; new-vertex, or nil if such a renaming does not
	       ;; produce a valid partial solution
	       ;;
	       ;; This loop is ugly, but has the advantage of doing a
	       ;; single pass over the edges and failing fast when an
	       ;; invalid edge label is detected. Since the search
	       ;; algorithm is dumb, we expect many failures here, so
	       ;; failing-fast is essential.
	       (loop
		  with new-edges = '()
		  with used-labels = (make-array (order labeled-graph)
						 :initial-element nil)
		  for (v1 v2 label) in (edges labeled-graph)
		  do (progn
		       (cond ((not (or (eql v1 vertex) (eql v2 vertex)))
			      (when (numberp label)
				(if (aref used-labels label)
				    (return-from next-graph nil)
				    (setf (aref used-labels label) t)))
			      (push (list v1 v2 label) new-edges))
			     (t (let ((other (if (eql v1 vertex) v2 v1)))
				  (if (numberp other)
				      (let ((new-label (abs (- new-vertex other))))
					(if (aref used-labels new-label)
					    (return-from next-graph nil)
					    (progn
					      (setf (aref used-labels new-label) t)
					      (push (list new-vertex other new-label)
						    new-edges))))
				      (push (list new-vertex other label)
					    new-edges))))))
		  finally (return (mk-labeled-graph
				   (substitute new-vertex
					       vertex
					       (vertices labeled-graph))
				   new-edges))))
	     (next-vs (next-v next-vs visited)
	       ;; Return the new set of next-vs by adding all vertices
	       ;; reachable from next-v to the current set.
	       (union
		(set-difference (adjacent-nodes adj-list next-v) visited)
		next-vs))
	     (recur (next-vs visited next-labels tried-labels labeled-graph)
	       (cond ((and (= (length visited) (order graph))
			   (null next-labels))
		      labeled-graph)
		     ((or (null next-vs) (null next-labels)) nil)
		     (t (let* ((next-v (car next-vs))
			       (next-graph (next-graph next-v (car next-labels) labeled-graph)))
			  (or (and next-graph
				   (recur (next-vs next-v (cdr next-vs) visited)
					  (cons next-v visited)
					  (append (cdr next-labels) tried-labels)
					  '()
					  next-graph))
			      (recur next-vs
				     visited
				     (cdr next-labels)
				     (cons (car next-labels) tried-labels)
				     labeled-graph)))))))
      (recur (take 1 (vertices graph))
	     '()
	     (range 1 (order graph))
	     '()
	     (convert-to 'labeled graph)))))

(defun graceful-p (labeled-graph)
  "Return t if LABELED-GRAPH is a graceful labeling."
  (let ((vs (vertices labeled-graph))
	(ls (mapcar #'third (edges labeled-graph))))
    (and (equal (sort (copy-list vs) #'<) (range 1 (length vs)))
	 (equal (sort (copy-list ls) #'<) (range 1 (1- (length vs))))
	 (every (lambda (e)
		  (destructuring-bind (v1 v2 label) e
		    (= label (abs (- v1 v2)))))
		(edges labeled-graph)))))

(defun von-koch-solution-p (graph labeled-graph)
  "Return t if LABELED-GRAPH is a graceful labeling of G."
  (and (isomorph-p graph labeled-graph)
       (graceful-p labeled-graph)))

(define-test von-koch-test
  (dolist (g (list (mk-graph '(a b c d e f g)
			     '((a b) (a d) (a g) (b c) (b e) (e f)))
		   (mk-graph '(a b c d e f g h i k m n p q)
			     '((a b) (a c) (a h) (a i) (a g) (d c) (d k) (e q)
			       (e c) (f c) (q m) (q n) (p n)))))
    (assert-true (von-koch-solution-p g (von-koch g)))))
