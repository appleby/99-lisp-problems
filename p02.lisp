;;;; (*) Find the last but one box of a list.
;;;; Example:
;;;; * (my-but-last '(a b c d))
;;;; (C D)
(in-package :99)

(defun my-but-last (lst)
  (if (cddr lst)
      (my-but-last (cdr lst))
      lst))
