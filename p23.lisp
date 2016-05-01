;;;; (**) Extract a given number of randomly selected elements from a
;;;; list. The selected items shall be returned in a list.
;;;; Example:
;;;; * (rnd-select '(a b c d e f g h) 3)
;;;; (E D A)

;;;; Hint: Use the built-in random number generator and the result of
;;;; problem P20.
(in-package :99-problems)

(defun random-sample (vector n)
  "Based on Algorithm S from Knuth. TAOCP, vol. 2. p. 142.
   Lifted from the source code of the book Practical Common Lisp.
   See Chapter27/database.lisp."
  (loop with selected = (make-array n :fill-pointer 0)
     for idx from 0
     when (= (length selected) n) return selected
     do
       (loop
          with to-select = (- n (length selected))
          for remaining = (- (length vector) idx)
          while (>= (* remaining (random 1.0)) to-select)
          do (incf idx))
       (vector-push (aref vector idx) selected)))

(defun rnd-select-knuth (lst n)
  ;; Heavy lifting done by random-sample. Just convert between list
  ;; and array types.
  (let ((vector (make-array (length lst) :initial-contents lst)))
    (loop for x across (random-sample vector n) collecting x)))

(defun rnd-select (lst n)
  ;; Implemented as suggested by hint
  (labels ((recur (ll selected)
	     (cond
	       ((null ll) nil)
	       ((= selected n) nil)
	       (t (let ((rnd-idx (1+ (random (length ll)))))
		    (cons (element-at ll rnd-idx) (recur (remove-at ll rnd-idx) (1+ selected))))))))
    (recur lst 0)))

(define-test rnd-select-test
  (dolist (test-fn (list #'rnd-select #'rnd-select-knuth))
    (assert-equal '() (funcall test-fn '() 0))
    (assert-equal '() (funcall test-fn '(a b c) 0))
    (let* ((input '(a b c d e f g h))
	   (result (funcall test-fn input 3)))
      (assert-eq 3 (length result))
      (assert-eq 3 (length (remove-duplicates result)))
      (assert-true (subsetp result input)))))
