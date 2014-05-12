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
  ((graph-list :accessor graph-list :initarg :data :initform '())))

(defclass undirected-graph (graph) ())
(defclass directed-graph (graph) ())
(defclass labeled-graph (graph) ())
(defclass labeled-undirected-graph (labeled-graph undirected-graph) ())
(defclass labeled-directed-graph (labeled-graph directed-graph) ())

(defun mk-graph (data)
  (make-instance 'undirected-graph :data (copy-seq data)))

(defun mk-digraph (data)
  (make-instance 'directed-graph :data (copy-seq data)))

(defun mk-labeled-graph (data)
  (make-instance 'labeled-undirected-graph :data (copy-seq data)))

(defun mk-labeled-digraph (data)
  (make-instance 'labeled-directed-graph :data (copy-seq data)))

(defgeneric adjacency (graph)
  (:documentation "Convert given GRAPH to an adjacency-list."))

(defmethod adjacency ((graph undirected-graph))
  (destructuring-bind (nodes edges) (graph-list graph)
    (loop for node in nodes collect
	 (list node (loop for (n1 n2) in edges
		       when (eq node n1) collect n2
		       when (eq node n2) collect n1)))))

(defmethod adjacency ((graph directed-graph))
  (destructuring-bind (nodes edges) (graph-list graph)
    (loop for node in nodes collect
	 (list node (loop for (n1 n2) in edges
		       when (eq node n1) collect n2)))))

(defmethod adjacency ((graph labeled-undirected-graph))
  (destructuring-bind (nodes edges) (graph-list graph)
    (loop for node in nodes collect
	 (list node (loop for (n1 n2 label) in edges
		       when (eq node n1) collect (list n2 label)
		       when (eq node n2) collect (list n1 label))))))

(defmethod adjacency ((graph labeled-directed-graph))
  (destructuring-bind (nodes edges) (graph-list graph)
    (loop for node in nodes collect
	 (list node (loop for (n1 n2 label) in edges
		       when (eq node n1) collect (list n2 label))))))

(defgeneric convert-to (to from)
  (:documentation "Convert between graph types."))

(defmethod convert-to ((_ (eql 'adjacency)) (graph graph))
  (make-instance (class-of graph) :data (adjacency graph)))

(defmethod convert-to ((_ (eql 'undirected)) (graph directed-graph))
  (destructuring-bind (directed-nodes directed-edges) (graph-list graph)
    (mk-graph
     (list directed-nodes
	   (loop for (n1 n2) in directed-edges
	      unless (member n1 edges :key #'second)
	      collect (list n1 n2) into edges
	      finally (return edges))))))

(defmethod convert-to ((_ (eql 'undirected)) (graph labeled-undirected-graph))
  graph)

(defmethod convert-to ((_ (eql 'undirected)) (graph labeled-directed-graph))
  graph)

(defun graph-equal (a b)
  (tree-equal (graph-list a) (graph-list b)))

(define-test graph-to-*-test
    (let ((graph (mk-graph '((b c d f g h k) ((b c) (b f) (c f) (f k) (g h)))))
	  (graph-adj (mk-graph '((b (c f)) (c (b f)) (d ()) (f (b c k)) (g (h)) (h (g)) (k (f)))))
	  (digraph (mk-digraph '((b c d f g h k) ((b c) (c b) (b f) (f b) (c f) (f c) (f k) (k f) (g h) (h g)))))
	  (digraph-adj (mk-digraph '((b (c f)) (c (b f)) (d ()) (f (b c k)) (g (h)) (h (g)) (k (f)))))
	  (labeled-graph (mk-labeled-graph '((b c d f g h k) ((b c 1) (b f 1) (c f 1) (f k 1) (g h 1)))))
	  (labeled-graph-adj
	   (mk-labeled-graph '((b ((c 1)(f 1))) (c ((b 1) (f 1))) (d ()) (f ((b 1) (c 1) (k 1))) (g ((h 1))) (h ((g 1))) (k ((f 1)))))))
      (assert-equality #'graph-equal graph-adj (convert-to 'adjacency graph))
      (assert-equality #'graph-equal digraph-adj (convert-to 'adjacency digraph))
      (assert-equality #'graph-equal labeled-graph-adj (convert-to 'adjacency labeled-graph))))
