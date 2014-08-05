;;;;  (**) Binary search trees (dictionaries)
;;;;
;;;; Write a function to construct a binary search tree from a list of
;;;; integer numbers.
;;;;
;;;; Example:
;;;; * (construct '(3 2 5 7 1))
;;;; (3 (2 (1 nil nil) nil) (5 nil (7 nil nil)))
;;;;
;;;; Then use this function to test the solution of the problem P56.
;;;;
;;;; Example:
;;;; * (symmetric '(5 3 18 1 4 12 21))
;;;; T
;;;; * (symmetric '(3 2 5 7 1))
;;;; T
;;;; * (symmetric '(3 2 5 7))
;;;; NIL

(in-package :99)

(defun add-element (e tree)
  (cond ((tree-empty-p tree)
	 (make-leaf-node e))
	((= e (tree-elem tree))
	 tree)
	((< e (tree-elem tree))
	 (list (tree-elem tree)
	       (add-element e (tree-left tree)) (tree-right tree)))
	((> e (tree-elem tree))
	 (list (tree-elem tree) (tree-left tree) (add-element e (tree-right tree))))))

(defun construct (lst)
  (loop
     for tree = (make-empty-tree) then (add-element e tree)
     for e in lst
     finally (return tree)))

(define-test construct-symmetric
  (let ((inputs '((t (5 3 18 1 4 12 21))
		  (t (3 2 5 7 1))
		  (nil (3 2 5 7)))))
    (loop for (expected input) in inputs
       do (assert-equal expected
			(symmetric (construct input))))))
