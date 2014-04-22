;;;; (*) Find the last but one box of a list.
;;;; Example:
;;;; * (my-but-last '(a b c d))
;;;; (C D)
(in-package :99)

(defun p02-but-last (lst)
  (if (cddr lst)
      (p02-but-last (cdr lst))
      lst))
