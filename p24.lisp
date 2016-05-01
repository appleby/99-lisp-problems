;;;; (*) Lotto: Draw N different random numbers from the set 1..M.
;;;; The selected numbers shall be returned in a list.
;;;; Example:
;;;; * (lotto-select 6 49)
;;;; (23 1 17 33 21 37)
(in-package :99-problems)

(defun lotto-select (take max)
  (rnd-select (range 1 max) take))

(define-test lotto-select-test
  (assert-equal '() (lotto-select 0 10))
  (let ((result (lotto-select 6 49)))
    (assert-eq 6 (length result))
    (assert-eq 6 (length (remove-duplicates result)))
    (assert-true (< 0 (apply #'min result)))
    (assert-true (> 50 (apply #'max result)))))
