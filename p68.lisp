;;;; (**) Preorder and inorder sequences of binary trees
;;;;
;;;; We consider binary trees with nodes that are identified by single
;;;; lower-case letters, as in the example of problem P67.
;;;;
;;;; a) Write functions preorder and inorder that construct the
;;;; preorder and inorder sequence of a given binary tree,
;;;; respectively. The results should be lists, e.g. (A B D E C F G)
;;;; for the preorder sequence of the example in problem P67.
;;;;
;;;; b) Can you write the inverse of preorder from problem part a) ;
;;;; i.e. given a preorder sequence, construct a corresponding tree?
;;;;
;;;; c) If both the preorder sequence and the inorder sequence of the
;;;; nodes of a binary tree are given, then the tree is determined
;;;; unambiguously. Write a function pre-in-tree that does the job.
(in-package :99-problems)

(defun preorder (tree)
  (if (tree-empty-p tree)
      *the-empty-tree*
      (append (list (tree-elem tree))
	      (preorder (tree-left tree))
	      (preorder (tree-right tree)))))

(defun inorder (tree)
  (if (tree-empty-p tree)
      *the-empty-tree*
      (append (inorder (tree-left tree))
	      (list (tree-elem tree))
	      (inorder (tree-right tree)))))

(defun split-preorder-on-inorder (preorder inorder-left inorder-right)
  (cond ((and (null inorder-left) (null inorder-right))
	 (list nil nil))
	((null inorder-left)
	 (list nil preorder))
	((null inorder-right)
	 (list preorder nil))
	(t
	 (split preorder (position (car inorder-right) preorder)))))

(defun pre-in-tree (preorder inorder)
  (if (or (null preorder) (null inorder))
      *the-empty-tree*
      (destructuring-bind (inorder-left inorder-right)
	  (split-sequence (car preorder) inorder)
	(destructuring-bind (preorder-left preorder-right)
	    (split-preorder-on-inorder (cdr preorder) inorder-left inorder-right)
	  (make-tree-node (car preorder)
			  (pre-in-tree preorder-left inorder-left)
			  (pre-in-tree preorder-right inorder-right))))))

(define-test pre-in-tree-test
  (let ((inputs (list *t1* *t2* *t3*)))
    (loop for input in inputs
       do (assert-equality #'tree-equal
			   input
			   (pre-in-tree (preorder input) (inorder input))))))
