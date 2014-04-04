;;;; (*) Find the number of elements of a list.

(defun p04-length (lst)
  (if (null lst)
      0
      (1+ (p04-recur (cdr lst)))))
