;;;; (***) Conversions
;;;;
;;;; Write functions to convert between the different graph
;;;; representations. With these functions, all representations are
;;;; equivalent; i.e. for the following problems you can always pick
;;;; freely the most convenient form. The reason this problem is rated
;;;; (***) is not because it's particularly difficult, but because
;;;; it's a lot of work to deal with all the special cases.
(in-package :99)

(defclass graph ()
  ((graph-data :accessor graph-data :initarg :data :initform '())))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass undirected-graph (graph) ())
  (defclass directed-graph (graph) ())
  (defclass labeled-graph (graph) ())
  (defclass labeled-undirected-graph (labeled-graph undirected-graph) ())
  (defclass labeled-directed-graph (labeled-graph directed-graph) ()))

(defmethod print-object ((object graph) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (graph-data) object
      (format stream "~a" graph-data))))

(defmacro defgraph-ctor (name class)
  `(defun ,name (nodes edges)
     (make-instance ',class :data (list nodes edges))))

(defgraph-ctor mk-graph undirected-graph)
(defgraph-ctor mk-digraph directed-graph)
(defgraph-ctor mk-labeled-graph labeled-undirected-graph)
(defgraph-ctor mk-labeled-digraph labeled-directed-graph)

(defun drop-labels (edges)
  (loop for (n1 n2 nil) in edges collect (list n1 n2)))

(defmacro defadjacency-method (graph-type)
  (with-gensyms (graph n1 n2 label node nodes edges)
    (let* ((is-labeled (subtypep graph-type 'labeled-graph))
	   (is-directed (subtypep graph-type 'directed-graph))
	   (edge-binding-form (if is-labeled `(,n1 ,n2 ,label) `(,n1 ,n2)))
	   (test-forms (if is-directed `((eq ,node ,n1)) `((eq ,node ,n1) (eq ,node ,n2))))
	   (collect-forms (if is-labeled `((list ,n2 ,label) (list ,n1 ,label)) `(,n2 ,n1)))
	   (test-collect-forms (mapcar #'list test-forms collect-forms)))
      `(defmethod adjacency ((,graph ,graph-type))
	 (destructuring-bind (,nodes ,edges) (graph-data ,graph)
	   (loop for ,node in ,nodes collect
		(list ,node (loop for ,edge-binding-form in ,edges
				 ,@(loop for (test-form collect-form) in test-collect-forms
				      append `(when ,test-form collect ,collect-form))))))))))

(defgeneric adjacency (graph)
  (:documentation "Convert given GRAPH to an adjacency-list."))

(defadjacency-method undirected-graph)
(defadjacency-method directed-graph)
(defadjacency-method labeled-undirected-graph)
(defadjacency-method labeled-directed-graph)

(defgeneric convert-to (to from)
  (:documentation "Convert between graph types."))

(defmethod convert-to ((_ (eql 'adjacency)) (graph graph))
  (make-instance (class-of graph) :data (adjacency graph)))

(defmethod convert-to ((_ (eql 'undirected)) (graph directed-graph))
  (destructuring-bind (directed-nodes directed-edges) (graph-data graph)
    (mk-graph directed-nodes
	      (loop for (n1 n2) in directed-edges
		 unless (member (list n2 n1) edges :test #'equal)
		 collect (list n1 n2) into edges
		 finally (return edges)))))

(defmethod convert-to ((_ (eql 'undirected)) (graph labeled-undirected-graph))
  (destructuring-bind (nodes edges) (graph-data graph)
    (mk-graph nodes (drop-labels edges))))

(defmethod convert-to ((_ (eql 'undirected)) (graph labeled-directed-graph))
  (destructuring-bind (nodes edges) (graph-data graph)
    (convert-to 'undirected (mk-digraph nodes (drop-labels edges)))))

(defmethod convert-to ((_ (eql 'directed)) (graph undirected-graph))
  (destructuring-bind (nodes edges) (graph-data graph)
    (mk-digraph nodes (loop for (n1 n2) in edges
			 collect (list n1 n2) collect (list n2 n1)))))

(defmethod convert-to ((_ (eql 'directed)) (graph labeled-undirected-graph))
  (destructuring-bind (nodes edges) (graph-data graph)
    (convert-to 'directed (mk-graph nodes (drop-labels edges)))))

(defmethod convert-to ((_ (eql 'directed)) (graph labeled-directed-graph))
  (destructuring-bind (nodes edges) (graph-data graph)
    (mk-digraph nodes (drop-labels edges))))

(defmethod convert-to ((_ (eql 'labeled)) (graph undirected-graph))
  (destructuring-bind (nodes edges) (graph-data graph)
    (mk-labeled-graph nodes (loop for label upfrom 1
			       for (n1 n2) in edges
			       collect (list n1 n2 label)))))

(defmethod convert-to ((_ (eql 'labeled)) (graph directed-graph))
  (convert-to 'labeled (convert-to 'undirected graph)))

(defmethod convert-to ((_ (eql 'labeled)) (graph labeled-directed-graph))
  (destructuring-bind (directed-nodes directed-edges) (graph-data graph)
    (mk-labeled-graph
     directed-nodes
     (loop
	for (n1 n2 label) in directed-edges
	unless (member (list n2 n1) edges :test #'equal :key #'butlast)
	collect (list n1 n2 label) into edges
	finally (return edges)))))

(defmethod convert-to ((_ (eql 'labeled-digraph)) (graph undirected-graph))
  (destructuring-bind (nodes edges) (graph-data graph)
    (mk-labeled-digraph nodes (loop for label upfrom 1
				 for (n1 n2) in edges
				 collect (list n1 n2 label)
				 collect (list n2 n1 label)))))

(defmethod convert-to ((_ (eql 'labeled-digraph)) (graph directed-graph))
  (destructuring-bind (nodes edges) (graph-data graph)
    (mk-labeled-digraph nodes (loop for label upfrom 1
			       for (n1 n2) in edges
			       collect (list n1 n2 label)))))

(defmethod convert-to ((_ (eql 'labeled-digraph)) (graph labeled-undirected-graph))
  (destructuring-bind (nodes edges) (graph-data graph)
    (mk-labeled-digraph nodes (loop for (n1 n2 label) in edges
				 collect (list n1 n2 label)
				 collect (list n2 n1 label)))))

(defun graph-equal (a b)
  (tree-equal (graph-data a) (graph-data b)))

(defmacro assert-graph-equal (graph-a graph-b &rest extras)
  `(assert-equality #'graph-equal ,graph-a ,graph-b ,extras))

;;; I believe there are errors in the representations of certain
;;; graphs given in the examples for this question (errors that also
;;; exist in the original prolog problems). For example, for the
;;; digraph
;;;     ( (r s t u v) ( (s r) (s u) (u r) (u s) (v u) ) ) the
;;; given adjacency list form is
;;;     ( (r ()) (s (r u)) (t ()) (u (r)) (v (u)) )
;;; which should probably instead be
;;;     ( (r ()) (s (r u)) (t ()) (u (r s)) (v (u)) )
;;;
;;; Also, the graph-expression-form of the labeled digraph is given
;;; as:
;;;     ( (k m p q) ( (m p 7) ...
;;; which should instead be
;;;     ( (k m p q) ( (m q 7) ...
(define-test graph-adjacency-test
    (let ((inputs '((undirected-graph
		     ((b c d f g h k) ((b c) (b f) (c f) (f k) (g h)))
		     ((b (c f)) (c (b f)) (d ()) (f (b c k)) (g (h)) (h (g)) (k (f))))
		    (directed-graph
		     ((r s t u v) ((s r) (s u) (u r) (u s) (v u)))
		     ((r ()) (s (r u)) (t ()) (u (r s)) (v (u))))
		    (labeled-undirected-graph
		     ((b c d f g h k) ((b c 1) (b f 2) (c f 3) (f k 4) (g h 5)))
		     ((b ((c 1) (f 2))) (c ((b 1) (f 3))) (d ()) (f ((b 2) (c 3) (k 4))) (g ((h 5))) (h ((g 5))) (k ((f 4)))))
		    (labeled-directed-graph
		     ((k m p q) ((m q 7) (p m 5) (p q 9)))
		     ((k ()) (m ((q 7))) (p ((m 5) (q 9))) (q ()))))))
      (loop
	 for (class graph-expression-form adjacency-list) in inputs
	 for gef = (make-instance class :data graph-expression-form)
	 for adj = (make-instance class :data adjacency-list)
	 do (assert-graph-equal adj (convert-to 'adjacency gef)))))

(define-test undirected-to-*-test
    (let ((graph (mk-graph '(b c d f g h k) '((b c) (b f) (c f) (f k) (g h))))
	  (digraph (mk-digraph '(b c d f g h k) '((b c) (c b) (b f) (f b) (c f) (f c) (f k) (k f) (g h) (h g))))
	  (labeled-graph (mk-labeled-graph '(b c d f g h k) '((b c 1) (b f 2) (c f 3) (f k 4) (g h 5))))
	  (labeled-digraph
	   (mk-labeled-digraph '(b c d f g h k)
			       '((b c 1) (c b 1) (b f 2) (f b 2) (c f 3) (f c 3) (f k 4) (k f 4) (g h 5) (h g 5)))))
      (assert-graph-equal digraph (convert-to 'directed graph))
      (assert-graph-equal labeled-graph (convert-to 'labeled graph))
      (assert-graph-equal labeled-digraph (convert-to 'labeled-digraph graph))

      (assert-graph-equal graph (convert-to 'undirected labeled-graph))
      (assert-graph-equal digraph (convert-to 'directed labeled-graph))
      (assert-graph-equal labeled-digraph (convert-to 'labeled-digraph labeled-graph))))

(define-test digraph-to-*-test
    (let ((digraph (mk-digraph '(r s t u v) '((s r) (s u) (u r) (u s) (v u))))
	  (graph (mk-graph '(r s t u v) '((s r) (s u) (u r) (v u))))
	  (labeled-graph (mk-labeled-graph '(r s t u v) '((s r 1) (s u 2) (u r 3) (v u 4))))
	  (labeled-digraph (mk-labeled-digraph '(r s t u v) '((s r 1) (s u 2) (u r 3) (u s 4) (v u 5)))))
      (assert-graph-equal graph (convert-to 'undirected digraph))
      (assert-graph-equal labeled-graph (convert-to 'labeled digraph))
      (assert-graph-equal labeled-digraph (convert-to 'labeled-digraph digraph))))

(define-test labeled-digraph-to-*-test
    (let ((labeled-digraph (mk-labeled-digraph '(k m p q) '((m q 7) (p m 5) (p q 9) (q m 11))))
	  (graph (mk-graph '(k m p q) '((m q) (p m) (p q))))
	  (digraph (mk-digraph '(k m p q) '((m q) (p m) (p q) (q m))))
	  (labeled-graph (mk-labeled-graph '(k m p q) '((m q 7) (p m 5) (p q 9)))))
      (assert-graph-equal graph (convert-to 'undirected labeled-digraph))
      (assert-graph-equal digraph (convert-to 'directed labeled-digraph))
      (assert-graph-equal labeled-graph (convert-to 'labeled labeled-digraph))))
