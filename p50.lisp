;;;; (***) Huffman code.
;;;;
;;;; First of all, consult a good book on discrete mathematics or
;;;; algorithms for a detailed description of Huffman codes!
;;;;
;;;; We suppose a set of symbols with their frequencies, given as a
;;;; list of (S F) elements. Example: ((a 45) (b 13) (c 12) (d 16) (e
;;;; 9) (f 5)). Our objective is to construct a list of (S C)
;;;; elements, where C is the Huffman code word for symbol S. In our
;;;; example, the result could be ((A "0") (B "101") (C "100") (D
;;;; "111") (E "1101") (F "1100")). The task shall be performed by a
;;;; function huffman defined as follows:
;;;;
;;;; (huffman F) returns the Huffman code table for the frequency
;;;; table F

(in-package :99)

(defclass node ()
  ((symbols :initarg :symbols :reader symbols)
   (weight :initarg :weight :reader weight)))

(defclass leaf-node (node)
  ())

(defclass tree-node (node)
  ((left :initarg :left :reader left-tree)
   (right :initarg :right :reader right-tree)))

(defun mkleaf (symbol weight)
  (make-instance 'leaf-node :symbols (list symbol) :weight weight))

(defun mktree (symbols weight left right)
  (make-instance 'tree-node :symbols symbols :weight weight :left left :right right))

(defun combine-nodes (node1 node2)
  (mktree (append (symbols node1) (symbols node2))
	  (+ (weight node1) (weight node2))
	  node1
	  node2))
  
(defun symbol-frequencies->priority-queue (symbol-frequencies)
  (loop
     with queue = (make-instance 'cl-heap:priority-queue)
     for (s f) in symbol-frequencies do (cl-heap:enqueue queue (mkleaf s f) f)
     finally (return queue)))

(defgeneric encode-symbol (symbol node)
  (:documentation "Encode a symbol from a huffman tree node."))

(defmethod encode-symbol (symbol (node leaf-node))
  (assert (eq symbol (car (symbols node))))
  "")

(defun next-bit-and-node (symbol node)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (cond ((member symbol (symbols (left-tree node))) (values "0" (left-tree node)))
	((member symbol (symbols (right-tree node))) (values "1" (right-tree node)))
	(t (error "Symbol '~a' does not exist in tree ~a" symbol node))))

(defmethod encode-symbol (symbol (node tree-node))
  (multiple-value-bind (next-bit next-node) (next-bit-and-node symbol node)
    (concatenate 'string next-bit (encode-symbol symbol next-node))))

(defun encode-symbols (symbols tree)
  (loop for s in symbols collect (list s (encode-symbol s tree))))

(defun make-huffman-tree (symbol-frequencies)
  (loop
     with queue = (symbol-frequencies->priority-queue symbol-frequencies)
     until (= 1 (cl-heap:queue-size queue))
     for node1 = (cl-heap:dequeue queue)
     for node2 = (cl-heap:dequeue queue)
     for new-node = (combine-nodes node1 node2)
     do (cl-heap:enqueue queue new-node (weight new-node))
     finally (return (cl-heap:dequeue queue))))

(defun huffman (symbol-frequencies)
  (encode-symbols (mapcar #'car symbol-frequencies)
		  (make-huffman-tree symbol-frequencies)))

(define-test huffman-known-values
  (let ((input '((a 45) (b 13) (c 12) (d 16) (e 9) (f 5)))
	(expected '((A "0") (B "101") (C "100") (D "111") (E "1101") (F "1100"))))
    (assert-equal expected (huffman input))))
