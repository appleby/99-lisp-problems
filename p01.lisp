;;;; (*) Find the last box of a list.
;;;; Example:
;;;; * (my-last '(a b c d))
;;;; (D)

(defun p01-last (lst)
  (if (cdr lst)
      (p01-last (cdr lst))
      lst))
