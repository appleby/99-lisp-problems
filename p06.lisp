;;;; (*) Find out whether a list is a palindrome.
;;;; A palindrome can be read forward or backward; e.g. (x a m a x).

;;; Tried to be clever and only iterate over half the list, but the
;;; calls to reverse and length end up making this version O(2.5n),
;;; whereas the the equal/reverse version below is O(2n).
(defun p06-palindrome-iter-p (lst)
  (loop
     for x in lst
     for y in (reverse lst)
     for i upto (ceiling (length lst) 2)
     when (not (equalp x y)) return nil
     finally (return t)))

(defun p06-palindrome-p (lst)
  (equal lst (reverse lst)))
