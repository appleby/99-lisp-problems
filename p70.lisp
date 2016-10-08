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
(in-package :99-problems)

(defun caret-string-lexer (caret-string &aux (caret-list (coerce caret-string 'list)))
  (lambda () (if (null caret-list)
	    (values nil nil)
	    (let ((char (pop caret-list)))
	      (cond ((char= char #\^) (values 'caret nil))
		    ((alpha-char-p char) (values 'symbol (intern (string char))))
		    (t (error "~S is neither '^' nor 'a-zA-Z'." char)))))))

(yacc:define-parser *caret-string-parser*
  (:start-symbol empty-or-mw-tree)
  (:terminals (caret symbol))
  (empty-or-mw-tree
   caret
   mw-tree)
  (mw-tree
   (symbol caret (lambda (s c) (declare (ignore c)) s))
   (symbol mw-tree+ caret
	   (lambda (s st+ c)
	     (declare (ignore c))
	     (cons s st+))))
  (mw-tree+
   (mw-tree #'list)
   (mw-tree+ mw-tree (lambda (x y) (append x (list y))))))

(defun caret-string->mw-tree (caret-string)
  (yacc:parse-with-lexer (caret-string-lexer caret-string) *caret-string-parser*))

(defun mw-tree->caret-string (mw-tree)
  (cond ((null mw-tree) "^")
	((symbolp mw-tree)
	 (format nil "~a^" mw-tree))
	(t
	 (format nil "~a~{~a~}^"
		 (car mw-tree)
		 (mapcar #'mw-tree->caret-string (cdr mw-tree))))))

(define-test mw-tree-caret-string-test
  (let ((inputs '(()
		  a
		  (a b)
		  (a b c)
		  (a (b c))
		  (a b c d)
		  (a (b c) d) (a b (c d))
		  (a (b c d))
		  (a (f g) c (b d e)))))
    (loop for mw-tree in inputs
       do (assert-equality
	   #'tree-equal
	   mw-tree
	   (caret-string->mw-tree (mw-tree->caret-string mw-tree))))))
