;;;; (*) Collect the internal nodes of a binary tree in a list
;;;;
;;;; An internal node of a binary tree has either one or two non-empty
;;;; successors. Write a function internals to collect them in a list.
;;;;
;;;; (internals tree) returns the list of internal nodes of the binary
;;;; tree tree.
(in-package :99)

(defun internals (tree)
  (cond ((tree-empty-p tree) *the-empty-tree*)
	((leaf-node-p tree) *the-empty-tree*)
	(t (cons (tree-elem tree) (append (internals (tree-left tree))
					  (internals (tree-right tree)))))))

(define-test internals-test
  (assert-equal '() (internals *the-empty-tree*))
  (assert-equal '() (internals *t2*))
  (assert-equal '(a b c f) (internals *t1*)))
