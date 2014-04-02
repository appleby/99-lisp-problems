;;;; (**) Extract a slice from a list.

;;;; Given two indices, I and K, the slice is the list containing the
;;;; elements between the I'th and K'th element of the original list
;;;; (both limits included). Start counting the elements with 1.

;;;; Example:
;;;; * (slice '(a b c d e f g h i k) 3 7)
;;;; (C D E F G)

(defun p18-slice (lst start end)
  (when (or (> start end) (not (plusp end)))
    (error "Invalid range (~d, ~d)" start end))
  (loop for elem in lst
        for idx from 1 upto end
        when (>= idx start) collect elem))
