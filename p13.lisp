;;;; (**) Run-length encoding of a list (direct solution).

;;;; Implement the so-called run-length encoding data compression
;;;; method directly. I.e. don't explicitly create the sublists
;;;; containing the duplicates, as in problem P09, but only count
;;;; them. As in problem P11, simplify the result list by replacing
;;;; the singleton lists (1 X) by X.

;;;; Example:
;;;; * (encode-direct '(a a a a b c c a a d e e e e))
;;;; ((4 A) B (2 C) (2 A) D (4 E))

;;; I already implemented this method as p11-encode-loop. Renaming it
;;; here to p13-encode.

(load "p11.lisp")
(defvar p13-encode)
(setf (symbol-function 'p13-encode) #'p11-encode-loop)
