;;;; (*) Find the number of elements of a list.

(defun p04-iter (lst)
  (let ((len 0))
    (dolist (x lst len)
      (declare (ignore x))
      (incf len))))

(defun p04-recur (lst)
  (if (null lst)
      0
      (1+ (p04-recur (cdr lst)))))
