;;;; (*) Lotto: Draw N different random numbers from the set 1..M.
;;;; The selected numbers shall be returned in a list.
;;;; Example:
;;;; * (lotto-select 6 49)
;;;; (23 1 17 33 21 37)
(in-package :99)

(defun p24-lotto-select (take max)
  (p23-rnd-select (p22-range 1 max) take))
