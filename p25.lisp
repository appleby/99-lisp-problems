;;;; (*) Generate a random permutation of the elements of a list.
;;;; Example:
;;;; * (rnd-permu '(a b c d e f))
;;;; (B A D C E F)
;;;; Hint: Use the solution of problem P23.
(in-package :99-problems)

(defun rnd-permu (lst)
  (rnd-select lst (length lst)))

(define-test rnd-permu-test
  (assert-equal '() (rnd-permu '()))
  (assert-equal '(a) (rnd-permu '(a)))
  (let* ((input '(a b c d e f))
	 (result (rnd-permu input)))
    (assert-equal (length input) (length result))
    (assert-equal (length input) (length (remove-duplicates result)))
    (assert-true (subsetp result input))))
