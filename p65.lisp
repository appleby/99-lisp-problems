;;;; (**) Layout a binary tree (2)
;;;;
;;;; An alternative layout method is depicted in the illustration
;;;; opposite. Find out the rules and write the corresponding Lisp
;;;; function. Hint: On a given level, the horizontal distance between
;;;; neighboring nodes is constant.
;;;;
;;;; Use the same conventions as in problem P64 and test your function
;;;; in an appropriate way.
(in-package :99)

(defun tree-height (tree)
  "Return the height of TREE, 0-indexed."
  (if (tree-empty-p tree)
      -1
      (1+ (max (tree-height (tree-left tree))
	       (tree-height (tree-right tree))))))

(defun layout-binary-tree-2 (tree)
  (let ((height (tree-height tree)))
    (labels ((layout (tree x y)
	       (if (tree-empty-p tree)
		   *the-empty-tree*
		   (let ((next-x-offset (expt 2 (- height y)))
			 (next-y (1+ y)))
		     (make-layout-node (tree-elem tree) x y
				       (layout (tree-left tree)
					       (- x next-x-offset)
					       next-y)
				       (layout (tree-right tree)
					       (+ x next-x-offset)
					       next-y))))))
      (layout tree (max 1 (1- (expt 2 height))) 1))))

(define-test layout-binary-tree-2-test
  (let ((inputs `((,*t1* (A 7 1 (B 3 2 (D 1 3 NIL NIL) (E 5 3 NIL NIL))
			    (C 11 2 NIL (F 13 3 (G 12 4 NIL NIL) NIL))))
		  (,*t2* (A 1 1 NIL NIL))
		  (,*t3* NIL)
		  ((N (K (C (A NIL NIL) (E (D NIL NIL) (G NIL NIL))) (M NIL NIL))
		      (U (P NIL (Q NIL NIL)) NIL))
		   (N 15 1
		      (K 7 2
			 (C 3 3
			    (A 1 4 NIL NIL)
			    (E 5 4 (D 4 5 NIL NIL) (G 6 5 NIL NIL)))
			 (M 11 3 NIL NIL))
		      (U 23 2 (P 19 3 NIL (Q 21 4 NIL NIL)) NIL)) ))))
    (loop for (input expected) in inputs
       do (assert-equality #'tree-equal expected (layout-binary-tree-2 input)))))
