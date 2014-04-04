;;;; (*) Create a list containing all integers within a given range.
;;;; If first argument is smaller than second, produce a list in
;;;; decreasing order.
;;;; Example:
;;;; * (range 4 9)
;;;; (4 5 6 7 8 9)

(defun p22-range (start end)
  (if (> start end)
      (loop for i from start downto end collecting i)
      (loop for i from start upto end collecting i)))

(defun p22-range-do (start end)
  (let ((succ (if (< start end) #'1+ #'1-))
	(done (if (< start end) #'> #'<)))
    (do ((i start (funcall succ i))
	 (acc nil (push i acc)))
	((funcall done i end) (nreverse acc)))))

(defun p22-range-mvb-loop (start end)
  (multiple-value-bind (succ done)
      (if (< start end) (values #'1+ #'>) (values #'1- #'<))
    (loop for i = start then (funcall succ i) until (funcall done i end) collect i)))
