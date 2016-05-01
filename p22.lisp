;;;; (*) Create a list containing all integers within a given range.
;;;; If first argument is smaller than second, produce a list in
;;;; decreasing order.
;;;; Example:
;;;; * (range 4 9)
;;;; (4 5 6 7 8 9)
(in-package :99-problems)

(defun range (start end)
  (if (> start end)
      (loop for i from start downto end collecting i)
      (loop for i from start upto end collecting i)))

(defun range-do (start end)
  (let ((succ (if (< start end) #'1+ #'1-))
	(done (if (< start end) #'> #'<)))
    (do ((i start (funcall succ i))
	 (acc nil (push i acc)))
	((funcall done i end) (nreverse acc)))))

(defun range-mvb-loop (start end)
  (multiple-value-bind (succ done)
      (if (< start end) (values #'1+ #'>) (values #'1- #'<))
    (loop for i = start then (funcall succ i) until (funcall done i end) collect i)))

(define-test range-test
  (dolist (test-fn (list #'range #'range-do #'range-mvb-loop))
    (assert-equal '(0) (funcall test-fn 0 0))
    (assert-equal '(0 1) (funcall test-fn 0 1))
    (assert-equal '(4 5 6 7 8 9) (funcall test-fn 4 9))))
