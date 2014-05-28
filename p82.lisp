;;; (*) Cycle from a given node
;;;
;;; Write a function (cycle g a) to find a closed path (cycle)
;;; starting at a given node a in the graph g. The function should
;;; return all cycles.
;;;
;;; Further clarification from the instructor:
;;;
;;; It depends on whether we are given a directed or undirected
;;; graph. Moreover, I think that directed cycles must have at least
;;; one edge, and undirected ones must have at least three edges.  In
;;; both cases, I think one should not repeat intermediate verticies.
(in-package :99)

(defun cycle (g a &optional (current-node a) visited directed)
  (if (and (> (length visited) 0) (eq a current-node))
      (and (> (length visited) (if directed 0 2)) (list (reverse (cons current-node visited))))
      (loop
	 for next-node in (set-difference (adjacent-nodes g current-node) (butlast visited))
	 for path = (cycle g a next-node (cons current-node visited) directed)
	 if path append path)))

(define-test cycle-test
  (let ((graphs-inputs
	 '((undirected
	    ((b (c f)) (c (b f)) (d ()) (f (b c k)) (g (h)) (h (g)) (k (f)))
	    ((b ((b c f b) (b f c b))) (c ((c f b c) (c b f c))) (g ()) (k ()) (d ())))
	   (directed
	    ((r ()) (s (r u)) (t ()) (u (r s)) (v (u)))
	    ((r ()) (s ((s u s))) (u ((u s u))) (t ()))))))
    (loop for (graph-type graph inputs) in graphs-inputs do
	 (loop for (start cycles) in inputs do
	      (assert-equality (lambda (x y) (set-equal x y :test #'equal))
			       cycles (cycle graph start start nil (eq graph-type 'directed)))))))
