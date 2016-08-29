;;; (**) Connected components
;;;
;;; Write a function that splits a graph into its connected
;;; components.
(in-package :99-problems)

(defun connected-components (graph)
  "Return a list of the sets of vertices that represent the connected
components of GRAPH."
  ;; Find all connected components via depth-first-search.
  (let ((remaining-vertices (vertices graph))
	(connected-components '()))
    (loop
       (when (null remaining-vertices)
	 (return connected-components))
       (let ((connected (depth-first-nodes graph (first remaining-vertices))))
	 (setf remaining-vertices (set-difference remaining-vertices connected))
	 (push connected connected-components)))))

(define-test connected-components-test
  (assert-equality
   #'set-set-equal
   '()
   (connected-components (mk-graph '() '())))
  (assert-equality
   #'set-set-equal
   '((a))
   (connected-components (mk-graph '(a) '())))
  (assert-equality
   #'set-set-equal
   '((a) (b))
   (connected-components (mk-graph '(a b) '())))
  (assert-equality
   #'set-set-equal
   '((a b))
   (connected-components (mk-graph '(a b) '((a b)))))
  (assert-equality
   #'set-set-equal
   '((a b) (c))
   (connected-components (mk-graph '(a b c) '((a b)))))
  (assert-equality
   #'set-set-equal
   '((a b c h) (d e f g i) (k l) (j))
   (connected-components (mk-graph '(a b c d e f g h i j k l)
				   '((a b) (a c) (b c) (c h)
				     (d e) (d f) (d g) (e f) (g i)
				     (k l))))))
