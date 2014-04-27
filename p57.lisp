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

(defun make-empty-tree ()
  '())

(defun make-leaf-node (x)
  (list x nil nil))

(defun p57-add-element (e tree)
  (cond ((null tree) (make-leaf-node e))
	((= e (car tree)) tree)
	((< e (car tree))
	 (list (car tree) (p57-add-element e (cadr tree)) (caddr tree)))
	((> e (car tree))
	 (list (car tree) (cadr tree) (p57-add-element e (caddr tree))))))

(defun p57-construct (lst)
  (loop
     for tree = (make-empty-tree) then (p57-add-element e tree)
     for e in lst
     finally (return tree)))

(define-test p57-construct-symmetric
  (let ((inputs '((t (5 3 18 1 4 12 21))
		  (t (3 2 5 7 1))
		  (nil (3 2 5 7)))))
    (loop for (expected input) in inputs
       do (assert-equal expected
			(p56-symmetric (p57-construct input))))))
