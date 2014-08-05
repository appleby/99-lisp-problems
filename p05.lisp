;;;; (*) Reverse a list
(in-package :99)

(defun my-reverse (lst)
  (let ((reversed '()))
    (dolist (x lst reversed)
      (push x reversed))))

(defun my-reverse-recur (lst)
  (labels ((recur (lst acc)
	     (if (null lst)
		 acc
		 (recur (cdr lst) (cons (car lst) acc)))))
    (recur lst '())))
