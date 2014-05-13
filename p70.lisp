;;;; (**) Tree construction from a node string
;;;;
;;;; We suppose that the nodes of a multiway tree contain single
;;;; characters. In the depth-first order sequence of its nodes, a
;;;; special character ^ has been inserted whenever, during the tree
;;;; traversal, the move is a backtrack to the previous level.
;;;;
;;;; By this rule, the tree in the figure opposite is represented as:
;;;; afg^^c^bd^e^^^
;;;;
;;;; Define the syntax of the string and write a function (tree
;;;; string) to construct the tree when the string is given. Work with
;;;; lists (instead of strings). Write also an inverse function.
(in-package :99)

(defun mw-tree-rep-action (rep)
  (if (not (listp rep))
      rep
      (mapcar (lambda (tree)
		(if (and (consp tree) (= 1 (length tree)))
		    (car tree)
		    tree))
	      rep)))

(defun mw-tree-action (sym children)
  (if children
      (cons sym children)
      sym))

(defgrammar caret-string-mw-tree
    :terminals ((sym "[a-z]|[A-Z]")
		(caret "\\^"))
    :start mw-tree
    :rules ((--> mw-tree
		 sym
		 (rep mw-tree :action (mw-tree-rep-action $1))
		 caret
		 :action (mw-tree-action (intern (cadr $1)) $2))))

(defun caret-string->mw-tree (caret-string)
  (parse-caret-string-mw-tree caret-string))

(defun mw-tree->caret-string (mw-tree)
  (cond ((null mw-tree) "")
	((symbolp mw-tree)
	 (format nil "~a^" mw-tree))
	(t
	 (format nil "~a~{~a~}^"
		 (car mw-tree)
		 (mapcar #'mw-tree->caret-string (cdr mw-tree))))))

(define-test mw-tree-caret-string-test
    (let ((inputs '(a
		    (a b)
		    (a b c) (a (b c))
		    (a b c d) (a (b c) d) (a b (c d)) (a (b c d))
		    (a (f g) c (b d e)))))
      (loop for mw-tree in inputs
	 do (assert-equality
	     #'tree-equal
	     mw-tree
	     (caret-string->mw-tree (mw-tree->caret-string mw-tree))))))
