;;; Test cases (can be used for other binary tree problems as well)
(in-package :99)

(defvar *the-empty-tree* nil)
(defvar *t1* '(a (b (d nil nil) (e nil nil)) (c nil (f (g nil nil) nil))))
(defvar *t2* '(a nil nil))
(defvar *t3* *the-empty-tree*)

(defun make-empty-tree ()
  *the-empty-tree*)

(defun make-leaf-node (&optional (symbol 'x))
  (make-tree-node symbol nil nil))

(defun make-tree-node (elem left right)
  (list elem left right))

(defun tree-elem (tree)
  (car tree))

(defun tree-left (tree)
  (cadr tree))

(defun tree-right (tree)
  (caddr tree))

(defun tree-empty-p (tree)
  (eq tree *the-empty-tree*))

(defun leaf-node-p (tree)
  (and (not (null tree))
       (null (tree-left tree))
       (null (tree-right tree))))
