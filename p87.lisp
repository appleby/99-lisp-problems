;;; (**) Depth-first order graph traversal
;;;
;;; Write a function that generates a depth-first order graph
;;; traversal sequence. The starting point should be specified, and
;;; the output should be a list of nodes that are reachable from this
;;; starting point (in depth-first order).
(in-package :99-problems)

(defun depth-first-nodes (graph
			  &optional (root-vertex (first (vertices graph)))
			  &aux visited (adj-list (adjacency graph)))
  "Return a depth-first pre-order traversal of GRAPH starting from ROOT-VERTEX."
  (labels ((visit (v)
	     (push v visited)
	     (unless (= (length visited) (length (vertices graph)))
	       (dolist (n (adjacent-nodes adj-list v))
		 (unless (member n visited)
		   (visit n))))))
    (and (not (null (vertices graph)))
	 (progn (visit root-vertex)
		(nreverse visited)))))

(define-test depth-first-nodes-test
  (assert-equal '() (depth-first-nodes (mk-graph '() '())))
  (assert-equal '(a) (depth-first-nodes (mk-graph '(a) '())))
  (assert-equal
   '(a b)
   (depth-first-nodes (mk-graph '(a b) '((a b))) 'a))
  (assert-equal
   '(b a)
   (depth-first-nodes (mk-graph '(a b) '((a b))) 'b))
  (assert-equal
   '(a b)
   (depth-first-nodes (mk-graph '(a b c) '((a b)))))
  (assert-equal
   '(a b c)
   (depth-first-nodes (mk-graph '(a b c) '((a b) (a c))) 'a))
  (assert-equal
   '(b a c)
   (depth-first-nodes (mk-graph '(a b c) '((a b) (a c))) 'b))
  (assert-equal
   '(a b d c)
   (depth-first-nodes (mk-graph '(a b c d) '((a b) (a c) (b d) (c d)))))
  (assert-equal
   '(a b d f e c g)
   (depth-first-nodes (mk-graph '(a b c d e f g)
				'((a b) (a c) (a e) (b d) (b f) (c g) (f e))))))
