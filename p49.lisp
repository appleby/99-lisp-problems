;;;; (**) Gray code.
;;;;
;;;; An n-bit Gray code is a sequence of n-bit strings constructed
;;;; according to certain rules. For example,
;;;;
;;;; n = 1: C(1) = ("0" "1").
;;;; n = 2: C(2) = ("00" "01" "11" "10").
;;;; n = 3: C(3) = ("000" "001" "011" "010" "110" "111" "101" "100").
;;;;
;;;; Find out the construction rules and write a function with the
;;;; following specification:
;;;;
;;;; (gray N) returns the N-bit Gray code
;;;;
;;;; Can you apply the method of "result caching" in order to make the
;;;; function more efficient, when it is to be used repeatedly?
(in-package :99)

(defun prepender (s)
  (lambda (x)
    (concatenate 'string s x)))

(defun p49-gray (n)
  (if (= n 1)
      (list "0" "1")
      (let* ((gray (p49-gray (1- n)))
	     (yarg (reverse gray)))
	(append (mapcar (prepender "0") gray)
		(mapcar (prepender "1") yarg)))))

(define-test p49-gray-know-values
  (let ((inputs '((1 . ("0" "1"))
		  (2 . ("00" "01" "11" "10"))
		  (3 . ("000" "001" "011" "010" "110" "111" "101" "100")))))
    (loop for (n . expected-codes) in inputs
       do (assert-equal expected-codes (p49-gray n)))))
