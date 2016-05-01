;;;;  (**) Dotstring representation of binary trees
;;;;
;;;; We consider again binary trees with nodes that are identified by
;;;; single lower-case letters, as in the example of problem P67. Such
;;;; a tree can be represented by the preorder sequence of its nodes
;;;; in which dots (.) are inserted where an empty subtree (nil) is
;;;; encountered during the tree traversal. For example, the tree
;;;; shown in problem P67 is represented as "ABD..E..C.FG...". First,
;;;; try to establish a syntax (BNF or syntax diagrams) and then write
;;;; functions tree and dotstring which do the conversion.
(in-package :99-problems)

(defgrammar dotstring-tree
    :terminals ((dot "\\.")
		(sym "[A-Z]|[a-z]"))
    :start tree
    :rules ((--> tree
		 (alt dot (seq sym tree tree))
		 :action (if (eq (car $1) 'DOT)
			     nil
			     (list (intern (cadar $1)) (cadr $1) (caddr $1))))))


(defun dotstring->tree (dot-string)
  (parse-dotstring-tree dot-string))

(defun tree->dotstring (tree)
  (if (tree-empty-p tree)
      "."
      (concatenate 'string
		   (string (tree-elem tree))
		   (tree->dotstring (tree-left tree))
		   (tree->dotstring (tree-right tree)))))


(define-test dotstring-conversion-test
    (let ((inputs (list *t1* *t2* *t3*)))
      (loop for input in inputs
	 do (assert-equality #'tree-equal
			     input
			     (dotstring->tree (tree->dotstring input))))))
