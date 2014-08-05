;;;; (*) Find the number of elements of a list.
(in-package :99)

(defun my-length (lst)
  (if (null lst)
      0
      (1+ (my-length (cdr lst)))))
