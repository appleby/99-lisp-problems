;;;; (**) Extract a given number of randomly selected elements from a
;;;; list. The selected items shall be returned in a list.
;;;; Example:
;;;; * (rnd-select '(a b c d e f g h) 3)
;;;; (E D A)

;;;; Hint: Use the built-in random number generator and the result of
;;;; problem P20.

(defun random-sample (vector n)
  "Based on Algorithm S from Knuth. TAOCP, vol. 2. p. 142.
   Lifted from the source code of the book Practical Common Lisp.
   See Chapter27/database.lisp."
  (loop with selected = (make-array n :fill-pointer 0)
     for idx from 0
     do
       (loop
          with to-select = (- n (length selected))
          for remaining = (- (length vector) idx)
          while (>= (* remaining (random 1.0)) to-select)
          do (incf idx))
       (vector-push (aref vector idx) selected)
     when (= (length selected) n) return selected))

(defun p23-rnd-select-knuth (lst n)
  ;; Heavy lifting done by random-sample. Just convert between list
  ;; and array types.
  (let ((vector (make-array (length lst) :initial-contents lst)))
    (loop for x across (random-sample vector n) collecting x)))

(load "p03.lisp")
(load "p20.lisp")

(defun p23-rnd-select (lst n)
  ;; Implemented as suggested by hint
  (labels ((recur (ll selected)
	     (cond
	       ((null ll) nil)
	       ((= selected n) nil)
	       (t (let ((rnd-idx (1+ (random (length ll)))))
		    (cons (p03-element-at ll rnd-idx) (recur (p20-remove-at ll rnd-idx) (1+ selected))))))))
    (recur lst 0)))
