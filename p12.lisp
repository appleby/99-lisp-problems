;;;; (**) Decode a run-length encoded list.

;;;; Given a run-length code list generated as specified in problem
;;;; P11. Construct its uncompressed version.

(defun p12-decode (rl-encoded-lst)
  (mapcan (lambda (x)
	    (if (consp x)
		(loop repeat (first x) collect (second x))
		(list x)))
	  rl-encoded-lst))
	
