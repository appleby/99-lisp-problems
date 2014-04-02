;;;;  (*) Modified run-length encoding.
;;;; Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

;;;; Example:
;;;; * (encode-modified '(a a a a b c c a a d e e e e))
;;;; ((4 A) B (2 C) (2 A) D (4 E))

(defun p09-pack (lst)
  (labels ((recur (lst acc)
	     (let ((this (car lst))    ; relies on (car '()) ==> '()
		   (next (cadr lst)))  ; likewise (cadr '()) ==> '()
	       (cond
		 ((null lst) acc)
		 ((equalp this next)
		  (recur (cdr lst) (cons this acc)))
		 (t (cons (cons this acc) (recur (cdr lst) '())))))))
    (recur lst '())))

(defun p11-encode (lst)
  (mapcar (lambda (sub-list)
	    (let ((len (length sub-list))
		  (elem (car sub-list)))
	      (if (> len 1)
		  (list len elem)
		  elem)))
	  (p09-pack lst)))

;;; The question specifically said to use the result of the last
;;; question; otherwise, it'd be better not to cons up the sublists
;;; and instead just keep a count of run-length and collect (N E)
;;; pairs in a loop, like so.
(defun p11-encode-loop (lst)
  (loop
       with n = 0
       for (cur . rest) on lst
       for next = (first rest)
       when (eql cur next) do (incf n)
       else
          when (zerop n) collect cur
          else collect (list (1+ n) cur) and do (setf n 0)))
