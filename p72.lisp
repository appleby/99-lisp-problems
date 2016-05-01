;;;; (*) Construct the bottom-up order sequence of the tree nodes
;;;;
;;;; Write a function (bottom-up mtree) which returns the bottom-up
;;;; sequence of the nodes of the multiway tree mtree as a Lisp list.

;;;; The following is a description taken from Algorithms on Trees and
;;;; Graphs by Gabriel Valiente (p. 129):
;;;;
;;;; In a bottom-up traversal of a tree, nodes are visited in order of
;;;; nondecreasing height. Nodes at the same height are visited in
;;;; order of nondecreasing depth, and nodes of the same height and
;;;; depth are visited in left-to-right order.
(in-package :99-problems)

(defun bottom-up (mw-tree)
  (labels ((annotate (tree depth)
	     (ecase (type-of tree)
	       ((symbol) (list (list 0 depth tree)))
	       ((cons)
		(destructuring-bind (height annotated-children)
		    (loop
		       for child in (cdr tree)
		       for annotated-child = (annotate child (1+ depth))
		       appending annotated-child into children
		       maximizing (1+ (apply #'max (mapcar #'car annotated-child))) into height
		       finally (return (list height children)))
		  (cons (list height depth (car tree)) annotated-children)))))
	   (un-annotate (lst)
	     (mapcar (lambda (annotated-node) (car (last annotated-node)))
		     lst))
	   (annotated-< (x y)
	     (or (< (car x) (car y))
		 (and (= (car x) (car y))
		      (< (cadr x) (cadr y))))))
    (un-annotate (stable-sort (annotate mw-tree 0) #'annotated-<))))

(define-test bottom-up-test
    (let ((inputs '(a (a)
		    (a b) (b a)
		    (a b c) (b c a)
		    (a (b c)) (c b a)
		    (a b c d) (b c d a)
		    (a b (c d)) (b d c a)
		    (a (b c) d) (d c b a)
		    (a (b c d)) (c d b a)
		    (a b c d e) (b c d e a)
		    (a b c (d e)) (b c e d a)
		    (a b (c d) e) (b e d c a)
		    (a (b c) d e) (d e c b a)
		    (a (b c) (d e)) (c e b d a)
		    (a b (c d e)) (b d e c a)
		    (a b (c (d e))) (b e d c a)
		    (a (b c d) e) (e c d b a)
		    (a (b (c d)) e) (e d c b a)
		    (a (b c d e)) (c d e b a)
		    (a (b c (d e))) (c e d b a)
		    (a (b (c d) e)) (e d c b a))))
      (loop for (input . rest) on inputs by #'cddr
	 do (assert-equal (car rest) (bottom-up input) input))))
