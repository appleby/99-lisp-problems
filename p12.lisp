;;;; (**) Decode a run-length encoded list.

;;;; Given a run-length code list generated as specified in problem
;;;; P11. Construct its uncompressed version.
(in-package :99)

(defun decode (rl-encoded-lst)
  (mapcan (lambda (x)
	    (if (consp x)
		(loop repeat (first x) collect (second x))
		(list x)))
	  rl-encoded-lst))

(define-test decode-test
  (assert-equal '() (decode '()))
  (assert-equal '(a a a a b c c a a d e e e e) (decode '((4 a) b (2 c) (2 a) d (4 e)))))
