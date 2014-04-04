;;;; (*) Remove the K'th element from a list.
;;;; Example:
;;;; * (remove-at '(a b c d) 2)
;;;; (A C D)

(defun p20-remove-at (lst n)
  (loop for elem in lst
        for idx upfrom 1
        unless (= idx n) collect elem))
