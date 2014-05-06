;;;; (**) A string representation of binary trees
;;;;
;;;; Somebody represents binary trees as strings of the following type
;;;; (see example opposite):
;;;;
;;;; a(b(d,e),c(,f(g,)))
;;;;
;;;; a) Write a Lisp function which generates this string
;;;; representation, if the tree is given as usual (as nil or (X L R)
;;;; expression). Then write a function which does this inverse;
;;;; i.e. given the string representation, construct the tree in the
;;;; usual form.
(in-package :99)

(defun tree->prefix-string (tree)
  (if (tree-empty-p tree)
      ""
      (let ((left (tree->prefix-string (tree-left tree)))
	    (right (tree->prefix-string (tree-right tree))))
	(format nil "~a~:[(~a,~a)~;~]"
		(tree-elem tree)
		(and (string= left "") (string= right ""))
		left
		right))))

(defun split-prefix-string (prefix-string)
  (loop
     with open-parens = 0
     for i upfrom 0
     for c across prefix-string
     when (char= c #\() do (incf open-parens)
     when (char= c #\)) do (decf open-parens)
     when (and (= open-parens 1) (char= c #\,))
     return (values (subseq prefix-string 1 i)
		    (subseq prefix-string (1+ i) (1- (length prefix-string))))))

(defun prefix-string->tree (prefix-string)
  (if (or (null prefix-string) (string= prefix-string ""))
      *the-empty-tree*
      (multiple-value-bind (left right) (split-prefix-string (subseq prefix-string 1))
	(make-tree-node (intern (subseq prefix-string 0 1))
			(prefix-string->tree left)
			(prefix-string->tree right)))))

(define-test prefix-string-tree-test
  (let ((inputs (list *t1* *t2* *t3*)))
    (loop for input in inputs
       do (assert-equality #'tree-equal
			   input
			   (prefix-string->tree (tree->prefix-string input))))))
