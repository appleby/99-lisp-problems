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

(defun length-less-than (n)
  (lambda (lst) (< (length lst) n)))

;;; Slightly more efficient iterative solution, for comparison.
;; (defun cycle (g a &optional directed)
;;   (let ((min-length (if directed 1 3))
;; 	(paths '()))
;;     (dolist (node (adjacent-nodes g a) paths)
;;       (dolist (path (path g node a))
;; 	(if (>= (length path) min-length)
;; 	    (pushnew (cons a path) paths))))))

(defun cycle (g a &optional directed)
  (mapcar (lambda (p) (cons a p))
	  (remove-if (length-less-than (if directed 1 3))
		     (mapcan (lambda (n) (path g n a)) (adjacent-nodes g a)))))

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
			       cycles (cycle graph start (eq graph-type 'directed)))))))
