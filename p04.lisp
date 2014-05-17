;;;; (*) Find the number of elements of a list.
(in-package :99)

(defun p04-length (lst)
  (if (null lst)
      0
      (1+ (p04-length (cdr lst)))))
