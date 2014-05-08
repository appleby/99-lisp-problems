;;;; (*) Determine the internal path length of a tree
;;;;
;;;; We define the internal path length of a multiway tree as the
;;;; total sum of the path lengths from the root to all nodes of the
;;;; tree. By this definition, the tree in the figure of problem P70
;;;; has an internal path length of 9. Write a function (ipl tree) to
;;;; compute it.
(in-package :99)

(defun ipl (mw-tree)
  (labels ((recur (tree height)
	     (ecase (type-of tree)
	       ((symbol) height)
	       ((cons)
		(reduce #'+
			(mapcar (lambda (tt) (recur tt (1+ height))) (cdr tree))
			:initial-value height)))))
    (recur mw-tree 0)))

(define-test ipl-test
    (let ((inputs '((0 a)
		    (1 (a b))
		    (2 (a b c))
		    (3 (a b c d))
		    (3 (a (b c)))
		    (9 (a (f g) c (b d e)))
		    (18 (a (b (c d) e) f (g (h i j)))))))
      (loop for (expected input) in inputs
	 do (assert-eq expected (ipl input)))))
