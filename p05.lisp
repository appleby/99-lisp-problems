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

(define-test my-reverse-test
  (dolist (reverse-fn (list #'my-reverse #'my-reverse-recur))
    (assert-equal '() (funcall reverse-fn '()))
    (assert-equal '(a) (funcall reverse-fn '(a)))
    (assert-equal '(e d c b a) (funcall reverse-fn '(a b c d e)))))
