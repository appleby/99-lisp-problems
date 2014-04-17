;;;; (**) Sorting a list of lists according to length of sublists
;;;;
;;;; a) We suppose that a list contains elements that are lists
;;;; themselves. The objective is to sort the elements of this list
;;;; according to their length. E.g. short lists first, longer lists
;;;; later, or vice versa.
;;;;
;;;; Example:
;;;; * (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
;;;; ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))
;;;;
;;;; b) Again, we suppose that a list contains elements that are lists
;;;; themselves. But this time the objective is to sort the elements
;;;; of this list according to their length frequency; i.e., in the
;;;; default, where sorting is done ascendingly, lists with rare
;;;; lengths are placed first, others with a more frequent length come
;;;; later.
;;;;
;;;; Example:
;;;; * (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
;;;; ((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))
;;;;
;;;; Note that in the above example, the first two lists in the result
;;;; have length 4 and 1, both lengths appear just once. The third and
;;;; forth list have length 3 which appears twice (there are two list
;;;; of this length). And finally, the last three lists have length
;;;; 2. This is the most frequent length.

(require :lisp-unit)
(load "p10.lisp")

(defun rl-encoded->alist (rl-encoded-list)
  (loop for lst in rl-encoded-list collect (cons (cadr lst) (car lst))))

(defun p28-lsort (lst)
  (sort lst #'< :key #'length))

(defun p28-lfsort (lst)
  (let ((freqencies (rl-encoded->alist (p10-encode
					(sort (mapcar #'length lst) #'<)))))
    (stable-sort lst #'< :key (lambda (x) (cdr (assoc (length x) freqencies))))))

(lisp-unit:define-test p28-lsort-known-solutions
  (let ((input '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
	(expected-solution '((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))))
    (lisp-unit:assert-equal expected-solution (p28-lsort (copy-list input)))))

(lisp-unit:define-test p28-lfsort-known-solutions
  (let ((input '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
	(expected-solution '((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))))
    (lisp-unit:assert-equal expected-solution (p28-lfsort (copy-list input)))))

(lisp-unit:define-test rl-encoded->alist-test
  (let ((input '((1 A) (3 B) (2 C) (7 D)))
	(expected-solution '((A . 1) (B . 3) (C . 2) (D . 7))))
    (lisp-unit:assert-equal expected-solution (rl-encoded->alist input))))
