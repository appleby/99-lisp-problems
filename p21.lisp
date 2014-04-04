;;;; (*) Insert an element at a given position into a list.
;;;; Example:
;;;; * (insert-at 'alfa '(a b c d) 2)
;;;; (A ALFA B C D)

(defun p21-insert-at (new-elem lst n)
  (loop for elem in lst
        for idx upfrom 1
        if (= idx n) collect new-elem
        else collect elem))
