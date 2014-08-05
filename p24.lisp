;;;; (*) Lotto: Draw N different random numbers from the set 1..M.
;;;; The selected numbers shall be returned in a list.
;;;; Example:
;;;; * (lotto-select 6 49)
;;;; (23 1 17 33 21 37)
(in-package :99)

(defun lotto-select (take max)
  (rnd-select (range 1 max) take))
