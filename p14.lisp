;;;; (*) Duplicate the elements of a list.
;;;; Example:
;;;; * (dupli '(a b c c d))
;;;; (A A B B C C C C D D)
(in-package :99)

(defun p14-dupli (lst)
  (mapcan (lambda (x) (list x x)) lst))
