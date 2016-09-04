;;; (**) Bipartite graphs
;;;
;;; Write a function that finds out whether a given graph is
;;; bipartite.
(in-package :99-problems)

;; The lazy solution, building on vertex-coloring from p85. This
;; solution is incorrect for a graph with >= 2 vertices and no edges,
;; because vertex-coloring returns the minimum possible coloring,
;; which would always be a 1-coloring for such a graph.
(defun probably-bipartite-p (graph)
  "Return t if the minimum coloring of GRAPH is a 2-coloring."
  (= 2 (length (remove-duplicates (mapcar #'cdr (vertex-coloring graph))))))

(defun bipartite-p (graph &aux (adj-list (adjacency graph)))
  "Return t if GRAPH is bipartite."
  ;; Proceed via breadth-first-search attempting to divide each
  ;; connected-component of GRAPH into two disjoint sets, U and V,
  ;; where member of U are connected only to vertices in V, and vice
  ;; versa.
  (labels ((member-of (s)
	     (lambda (n) (member n s)))
	   (recur (u v candidate-us candidate-vs remaining)
	     (cond ((and (null candidate-us) (null candidate-vs) (null remaining))
		    t)
		   ((and (null candidate-us) (null candidate-vs))
		    ;; We've found a connected-component that is
		    ;; bipartite. Arbitrarily pick an un-visited
		    ;; vertex and start over. The idea here is that if
		    ;; all connected-components of G are bipartite,
		    ;; then G is also bipartite. We can ignore
		    ;; unconnected vertices, as they can be always be
		    ;; added to either U or V arbitrarily.
		    (recur '() '() (list (car remaining)) '() (cdr remaining)))
		   ((null candidate-us)
		    ;; Swap U and V.
		    (recur v u candidate-vs '() remaining))
		   (t
		    ;; Pick a vertex from candidate-us. If any of the
		    ;; candidate vertex's neighbors is in U, give
		    ;; up. Otherwise, add the candidate vertex to U,
		    ;; and add all neighbors not already in V to
		    ;; candidate-vs and continue.
		    (let* ((current (car candidate-us))
			   (neighbors (adjacent-nodes adj-list current))
			   (next-vs (set-difference neighbors v)))
		      (and (notany (member-of u) next-vs)
			   (recur (cons current u)
				  v
				  (cdr candidate-us)
				  next-vs
				  (set-difference remaining next-vs))))))))
    (and (> (length (vertices graph)) 1)
	 (recur '() '() (list (car (vertices graph))) '() (cdr (vertices graph))))))

(define-test bipartite-p-test
  (dolist (bipartite (list (mk-graph '(a b) '())
			   (mk-graph '(a b c) '((a b)))
			   (mk-graph '(a b c) '((a b) (a c)))
			   (mk-graph '(a b c d) '((a b) (a c) (a d)))
			   (mk-graph '(a b c d) '((a b) (a c) (b d) (c d)))
			   (mk-graph '(a b c d e)
				     '((a b) (a d) (a e)
				       (b c)
				       (c d) (c e)))
			   (mk-graph '(a b c d e f g h)
				     '((a f) (a g) (a h)
				       (b f) (b g) (b h)
				       (c f) (c g) (c h)
				       (d f) (d g) (d h)
				       (e f) (e g) (e h)))))
    (assert-true (bipartite-p bipartite))
    (unless (and (> (length (vertices bipartite)) 1)
		 (zerop (length (edges bipartite))))
      ;; Probably-bipartite-p gives an incorrect answer in this
      ;; case. See the comment above.
      (assert-true (probably-bipartite-p bipartite))))
  (dolist (not-bipartite (list (mk-graph '() '())
			       (mk-graph '(a) '())
			       (mk-graph '(a b c) '((a b) (a c) (b c)))
			       (mk-graph '(a b c d) '((a b) (a c) (a d) (b d)))))
    (assert-false (bipartite-p not-bipartite))
    (assert-false (probably-bipartite-p not-bipartite))))
