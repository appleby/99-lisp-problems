;;;; (*) Run-length encoding of a list.

;;;; Use the result of problem P09 to implement the so-called
;;;; run-length encoding data compression method. Consecutive
;;;; duplicates of elements are encoded as lists (N E) where N is the
;;;; number of duplicates of the element E.

;;;; Example:
;;;; * (encode '(a a a a b c c a a d e e e e))
;;;; ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
(in-package :99-problems)

(defun encode (lst)
  (mapcar (lambda (sub-list)
	    (let ((len (length sub-list))
		  (elem (car sub-list)))
	      (list len elem)))
	  (pack lst)))

;;; The question specifically said to use the result of the last
;;; question; otherwise, it'd be better not to cons up the sublists
;;; and instead just keep a count of run-length and collect (N E)
;;; pairs in a loop, like so.
(defun encode-loop (lst)
  (loop
       with n = 0
       for (cur . rest) on lst
       for next = (first rest)
       when (eql cur next) do (incf n)
       else collect (list (1+ n) cur)
            and do (setf n 0)))

(define-test encode-test
  (dolist (test-fn (list #'encode #'encode-loop))
    (assert-equal '() (funcall test-fn '()))
    (assert-equal '((4 a) (1 b) (2 c) (2 a) (1 d)(4 e))
		  (funcall test-fn '(a a a a b c c a a d e e e e)))))
