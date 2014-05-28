;;;; (**) Path from one node to another one
;;;;
;;;; Write a function (path g a b) to return an acyclic path from node
;;;; a to node b in the graph g. The function should return all paths.
(in-package :99)

(defun adjacent-nodes (adj-list n)
  "Return nodes adjacent to N in given adjacency list ADJ-LIST."
  (loop for (node adjacents) in adj-list if (eq n node) return adjacents))

(defun path (g a b &optional visited)
  (if (eq a b)
      (list (reverse (cons a visited)))
      (loop
	 for next-node in (set-difference (adjacent-nodes g a) visited)
	 for path = (path g next-node b (cons a visited))
	 if path append path)))

(define-test path-test
  (let ((graphs-inputs
	 '((((b (c f)) (c (b f)) (d ()) (f (b c k)) (g (h)) (h (g)) (k (f)))
	    ((b b ((b))) (b k ((b f k) (b c f k))) (b d ()) (g h ((g h)))))
	   (((r ()) (s (r u)) (t ()) (u (r)) (v (u)))
	    ((r u ()) (u r ((u r))) (s r ((s r) (s u r))) (t u ()) (u v ()) (v u ((v u))))))))
    (loop for (graph inputs) in graphs-inputs do
	 (loop for (from to paths) in inputs do
	      (assert-equality (lambda (x y) (set-equal x y :test #'equal))
			       paths (path graph from to))))))
