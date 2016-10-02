;;; (**) Eight queens problem
;;;
;;; This is a classical problem in computer science. The objective is
;;; to place eight queens on a chessboard so that no two queens are
;;; attacking each other; i.e., no two queens are in the same row, the
;;; same column, or on the same diagonal.
;;;
;;; Hint: Represent the positions of the queens as a list of numbers
;;; 1..N. Example: [4,2,7,3,6,8,5,1] means that the queen in the first
;;; column is in row 4, the queen in the second column is in row 2,
;;; etc. Use the generate-and-test paradigm.
(in-package :99-problems)

(defun n-queens-naive (&optional (n 8))
  ;; Naive solution that generates all row-permutations and filters
  ;; non-solutions.
  (assert (>= n 0))
  (mapcar #'row-list->algebraic
	  (remove-if-not #'n-queens-solution-p (permutations (range 0 (1- n))))))

(defun n-queens (&optional (n 8))
  "Return a list of all n-queens solutions."
  ;; Slightly less naive than n-queens-naive. Rather than generating
  ;; all permutations and filtering, this version proceeds to generate
  ;; permutations via depth-first-search, pruning the tree and
  ;; backtracking as soon as it finds an invalid partial solution,
  ;; which cuts down the search space significantly. This is version
  ;; is still not especially fast. The data structures are immutable
  ;; and this version makes no attempt to minimize consing. We could
  ;; reduce the work further by only looking for fundamental
  ;; solutions, and generating the rest via reflection and rotation.
  (assert (>= n 0))
  (and (plusp n) ; handle n=0 as a special case.
       (mapcar #'row-list->algebraic
	       (n-queens-dfs (make-instance 'n-queens-solution-state
					    :n n
					    :remaining (range 0 (1- n))
					    :partial-solution nil)))))

(defun n-queens-partial-solution-p (queen-rows)
  "Return t if QUEEN-ROWS is a valid partial solution."
  ;; A solution is valid if no two queens occupy the same row, column,
  ;; or diagonal. The rows check could be omitted for the purposes of
  ;; this implementation. Both n-queens and n-queens-naive construct
  ;; permutations such that no two queens will ever occupy the same
  ;; row. We leave the row check here as a sanity check. Note that a
  ;; corresponding column check is uneccessary because each item in
  ;; queen-rows represents a distinct column by definition.
  (loop
     with rows = (make-hash-table)
     with positive-diagonals = (make-hash-table)
     with negative-diagonals = (make-hash-table)
     for col upfrom 0
     for row in queen-rows
     for pos-key = (+ row col)
     for neg-key = (- row col)
     never (or (hash-set-p row rows)
	       (hash-set-p pos-key positive-diagonals)
	       (hash-set-p neg-key negative-diagonals))
     do (progn (setf (gethash row rows) t)
	       (setf (gethash pos-key positive-diagonals) t)
	       (setf (gethash neg-key negative-diagonals) t))))

(defun n-queens-dfs (ss)
  (or (ss-solution ss)
      (and (ss-is-partial-solution-p ss)
	   (alexandria:mappend
	    (lambda (next) (n-queens-dfs (ss-next ss next)))
	    (ss-remaining ss)))))

(defun n-queens-solution-p (queen-rows)
  (and (n-queens-partial-solution-p queen-rows)
       (let ((n (length queen-rows)))
	 (every (lambda (r) (< r n)) queen-rows))))

(defun column-number->symbol (n)
  ;; Convert a 0-indexed column number to a symbol.
  ;; 0 -> A, 1 -> B, ..., 8 -> H, ..., 26 -> Z.
  (assert (< n 256)) ; sanity check
  (intern (string (code-char (+ (char-code #\A) n)))))

(defun row-list->algebraic (row-list)
  ;; Convert a solution in row-list form to algebraic notation. A
  ;; row-list is list of integers where the ith list element
  ;; represents the 0-indexed row number containing the queen for
  ;; column i. In other words '(0 1 2 3 4 5 6 7 8) represents queens
  ;; along the diagonal at board positions (0,0),(1,1)...(8,8).
  ;;
  ;; Algebraic notation is the standard notation for referencing chess
  ;; positions, with columns labeled a-h from left to right, and rows
  ;; numbered 1-8 from bottom to top. Thus, the square (0,0) in
  ;; row-column format corresponds to (a 1) in algebraic notation, and
  ;; (8,8) maps to (h 8).
  (zip (mapcar #'column-number->symbol (range 0 (1- (length row-list))))
       (mapcar #'1+ row-list)))

(defclass n-queens-solution-state ()
  ((n :initarg :n :reader ss-n)
   (remaining
    :initarg :remaining
    :reader ss-remaining)
   (partial-solution
    :initarg :partial-solution
    :reader ss-partial-solution)))

;;; These could be generic functions, but there is no need.
(defun ss-is-solution-p (ss)
  "Return t if SS is a solution."
  (and (= (ss-n ss) (length (ss-partial-solution ss)))
       (n-queens-solution-p (ss-partial-solution ss))))

(defun ss-is-partial-solution-p (ss)
  "Return t if SS is a partial solution."
  (n-queens-partial-solution-p (ss-partial-solution ss)))

(defun ss-solution (ss)
  "Return the solution that corresponds to this state, or nil."
  (and (ss-is-solution-p ss)
       (list (ss-partial-solution ss))))

(defun ss-next (ss next-move)
  "Return the state derived by taking NEXT-MOVE from SS."
  (make-instance 'n-queens-solution-state
		 :n (ss-n ss)
		 :remaining (remove next-move (ss-remaining ss))
		 :partial-solution (append (ss-partial-solution ss)
					   (list next-move))))

(defun n-queens-solution-equal (a b)
  (set-equal a b :test (lambda (x y) (set-equal x y :test #'equal))))

;;; Test values generated with a version of Jeff Somers' program
;;; modified to print results in sexp form.
;;;
;;;    http://www.jsomers.com/nqueen_demo/nqueens.html
(define-test n-queens
  (dolist (n-queens-impl (list #'n-queens #'n-queens-naive))
    (assert-equality #'n-queens-solution-equal '() (funcall n-queens-impl 0))
    (assert-equality #'n-queens-solution-equal '() (funcall n-queens-impl 2))
    (assert-equality #'n-queens-solution-equal '() (funcall n-queens-impl 3))
    (assert-equality #'n-queens-solution-equal
		     '(((a 1))) (funcall n-queens-impl 1))
    (assert-equality #'n-queens-solution-equal
		     (funcall n-queens-impl 4)
		     '(((b 1) (d 2) (a 3) (c 4))
		       ((c 1) (a 2) (d 3) (b 4))))
    (assert-equality #'n-queens-solution-equal
		     (funcall n-queens-impl 5)
		     '(((a 1) (c 2) (e 3) (b 4) (d 5))
		       ((e 1) (c 2) (a 3) (d 4) (b 5))
		       ((a 1) (d 2) (b 3) (e 4) (c 5))
		       ((e 1) (b 2) (d 3) (a 4) (c 5))
		       ((b 1) (d 2) (a 3) (c 4) (e 5))
		       ((d 1) (b 2) (e 3) (c 4) (a 5))
		       ((b 1) (e 2) (c 3) (a 4) (d 5))
		       ((d 1) (a 2) (c 3) (e 4) (b 5))
		       ((c 1) (a 2) (d 3) (b 4) (e 5))
		       ((c 1) (e 2) (b 3) (d 4) (a 5))))
    (assert-equality #'n-queens-solution-equal
		     (funcall n-queens-impl 6)
		     '(((b 1) (d 2) (f 3) (a 4) (c 5) (e 6))
		       ((e 1) (c 2) (a 3) (f 4) (d 5) (b 6))
		       ((c 1) (f 2) (b 3) (e 4) (a 5) (d 6))
		       ((d 1) (a 2) (e 3) (b 4) (f 5) (c 6))))
    (assert-equality #'n-queens-solution-equal
		     (funcall n-queens-impl 7)
		     '(((a 1) (c 2) (e 3) (g 4) (b 5) (d 6) (f 7))
		       ((g 1) (e 2) (c 3) (a 4) (f 5) (d 6) (b 7))
		       ((a 1) (d 2) (g 3) (c 4) (f 5) (b 6) (e 7))
		       ((g 1) (d 2) (a 3) (e 4) (b 5) (f 6) (c 7))
		       ((a 1) (e 2) (b 3) (f 4) (c 5) (g 6) (d 7))
		       ((g 1) (c 2) (f 3) (b 4) (e 5) (a 6) (d 7))
		       ((a 1) (f 2) (d 3) (b 4) (g 5) (e 6) (c 7))
		       ((g 1) (b 2) (d 3) (f 4) (a 5) (c 6) (e 7))
		       ((b 1) (d 2) (a 3) (g 4) (e 5) (c 6) (f 7))
		       ((f 1) (d 2) (g 3) (a 4) (c 5) (e 6) (b 7))
		       ((b 1) (d 2) (f 3) (a 4) (c 5) (e 6) (g 7))
		       ((f 1) (d 2) (b 3) (g 4) (e 5) (c 6) (a 7))
		       ((b 1) (e 2) (a 3) (d 4) (g 5) (c 6) (f 7))
		       ((f 1) (c 2) (g 3) (d 4) (a 5) (e 6) (b 7))
		       ((b 1) (e 2) (c 3) (a 4) (g 5) (d 6) (f 7))
		       ((f 1) (c 2) (e 3) (g 4) (a 5) (d 6) (b 7))
		       ((b 1) (e 2) (g 3) (d 4) (a 5) (c 6) (f 7))
		       ((f 1) (c 2) (a 3) (d 4) (g 5) (e 6) (b 7))
		       ((b 1) (f 2) (c 3) (g 4) (d 5) (a 6) (e 7))
		       ((f 1) (b 2) (e 3) (a 4) (d 5) (g 6) (c 7))
		       ((b 1) (g 2) (e 3) (c 4) (a 5) (f 6) (d 7))
		       ((f 1) (a 2) (c 3) (e 4) (g 5) (b 6) (d 7))
		       ((c 1) (a 2) (f 3) (b 4) (e 5) (g 6) (d 7))
		       ((e 1) (g 2) (b 3) (f 4) (c 5) (a 6) (d 7))
		       ((c 1) (a 2) (f 3) (d 4) (b 5) (g 6) (e 7))
		       ((e 1) (g 2) (b 3) (d 4) (f 5) (a 6) (c 7))
		       ((c 1) (e 2) (g 3) (b 4) (d 5) (f 6) (a 7))
		       ((e 1) (c 2) (a 3) (f 4) (d 5) (b 6) (g 7))
		       ((c 1) (f 2) (b 3) (e 4) (a 5) (d 6) (g 7))
		       ((e 1) (b 2) (f 3) (c 4) (g 5) (d 6) (a 7))
		       ((c 1) (g 2) (b 3) (d 4) (f 5) (a 6) (e 7))
		       ((e 1) (a 2) (f 3) (d 4) (b 5) (g 6) (c 7))
		       ((c 1) (g 2) (d 3) (a 4) (e 5) (b 6) (f 7))
		       ((e 1) (a 2) (d 3) (g 4) (c 5) (f 6) (b 7))
		       ((d 1) (a 2) (c 3) (f 4) (b 5) (g 6) (e 7))
		       ((d 1) (g 2) (e 3) (b 4) (f 5) (a 6) (c 7))
		       ((d 1) (a 2) (e 3) (b 4) (f 5) (c 6) (g 7))
		       ((d 1) (g 2) (c 3) (f 4) (b 5) (e 6) (a 7))
		       ((d 1) (b 2) (g 3) (e 4) (c 5) (a 6) (f 7))
		       ((d 1) (f 2) (a 3) (c 4) (e 5) (g 6) (b 7))))
    (assert-equality #'n-queens-solution-equal
		     (funcall n-queens-impl 8)
		     '(((a 1) (e 2) (h 3) (f 4) (c 5) (g 6) (b 7) (d 8))
		       ((h 1) (d 2) (a 3) (c 4) (f 5) (b 6) (g 7) (e 8))
		       ((a 1) (f 2) (h 3) (c 4) (g 5) (d 6) (b 7) (e 8))
		       ((h 1) (c 2) (a 3) (f 4) (b 5) (e 6) (g 7) (d 8))
		       ((a 1) (g 2) (d 3) (f 4) (h 5) (b 6) (e 7) (c 8))
		       ((h 1) (b 2) (e 3) (c 4) (a 5) (g 6) (d 7) (f 8))
		       ((a 1) (g 2) (e 3) (h 4) (b 5) (d 6) (f 7) (c 8))
		       ((h 1) (b 2) (d 3) (a 4) (g 5) (e 6) (c 7) (f 8))
		       ((b 1) (d 2) (f 3) (h 4) (c 5) (a 6) (g 7) (e 8))
		       ((g 1) (e 2) (c 3) (a 4) (f 5) (h 6) (b 7) (d 8))
		       ((b 1) (e 2) (g 3) (a 4) (c 5) (h 6) (f 7) (d 8))
		       ((g 1) (d 2) (b 3) (h 4) (f 5) (a 6) (c 7) (e 8))
		       ((b 1) (e 2) (g 3) (d 4) (a 5) (h 6) (f 7) (c 8))
		       ((g 1) (d 2) (b 3) (e 4) (h 5) (a 6) (c 7) (f 8))
		       ((b 1) (f 2) (a 3) (g 4) (d 5) (h 6) (c 7) (e 8))
		       ((g 1) (c 2) (h 3) (b 4) (e 5) (a 6) (f 7) (d 8))
		       ((b 1) (f 2) (h 3) (c 4) (a 5) (d 6) (g 7) (e 8))
		       ((g 1) (c 2) (a 3) (f 4) (h 5) (e 6) (b 7) (d 8))
		       ((b 1) (g 2) (c 3) (f 4) (h 5) (e 6) (a 7) (d 8))
		       ((g 1) (b 2) (f 3) (c 4) (a 5) (d 6) (h 7) (e 8))
		       ((b 1) (g 2) (e 3) (h 4) (a 5) (d 6) (f 7) (c 8))
		       ((g 1) (b 2) (d 3) (a 4) (h 5) (e 6) (c 7) (f 8))
		       ((b 1) (h 2) (f 3) (a 4) (c 5) (e 6) (g 7) (d 8))
		       ((g 1) (a 2) (c 3) (h 4) (f 5) (d 6) (b 7) (e 8))
		       ((c 1) (a 2) (g 3) (e 4) (h 5) (b 6) (d 7) (f 8))
		       ((f 1) (h 2) (b 3) (d 4) (a 5) (g 6) (e 7) (c 8))
		       ((c 1) (e 2) (b 3) (h 4) (a 5) (g 6) (d 7) (f 8))
		       ((f 1) (d 2) (g 3) (a 4) (h 5) (b 6) (e 7) (c 8))
		       ((c 1) (e 2) (b 3) (h 4) (f 5) (d 6) (g 7) (a 8))
		       ((f 1) (d 2) (g 3) (a 4) (c 5) (e 6) (b 7) (h 8))
		       ((c 1) (e 2) (g 3) (a 4) (d 5) (b 6) (h 7) (f 8))
		       ((f 1) (d 2) (b 3) (h 4) (e 5) (g 6) (a 7) (c 8))
		       ((c 1) (e 2) (h 3) (d 4) (a 5) (g 6) (b 7) (f 8))
		       ((f 1) (d 2) (a 3) (e 4) (h 5) (b 6) (g 7) (c 8))
		       ((c 1) (f 2) (b 3) (e 4) (h 5) (a 6) (g 7) (d 8))
		       ((f 1) (c 2) (g 3) (d 4) (a 5) (h 6) (b 7) (e 8))
		       ((c 1) (f 2) (b 3) (g 4) (a 5) (d 6) (h 7) (e 8))
		       ((f 1) (c 2) (g 3) (b 4) (h 5) (e 6) (a 7) (d 8))
		       ((c 1) (f 2) (b 3) (g 4) (e 5) (a 6) (h 7) (d 8))
		       ((f 1) (c 2) (g 3) (b 4) (d 5) (h 6) (a 7) (e 8))
		       ((c 1) (f 2) (d 3) (a 4) (h 5) (e 6) (g 7) (b 8))
		       ((f 1) (c 2) (e 3) (h 4) (a 5) (d 6) (b 7) (g 8))
		       ((c 1) (f 2) (d 3) (b 4) (h 5) (e 6) (g 7) (a 8))
		       ((f 1) (c 2) (e 3) (g 4) (a 5) (d 6) (b 7) (h 8))
		       ((c 1) (f 2) (h 3) (a 4) (d 5) (g 6) (e 7) (b 8))
		       ((f 1) (c 2) (a 3) (h 4) (e 5) (b 6) (d 7) (g 8))
		       ((c 1) (f 2) (h 3) (a 4) (e 5) (g 6) (b 7) (d 8))
		       ((f 1) (c 2) (a 3) (h 4) (d 5) (b 6) (g 7) (e 8))
		       ((c 1) (f 2) (h 3) (b 4) (d 5) (a 6) (g 7) (e 8))
		       ((f 1) (c 2) (a 3) (g 4) (e 5) (h 6) (b 7) (d 8))
		       ((c 1) (g 2) (b 3) (h 4) (e 5) (a 6) (d 7) (f 8))
		       ((f 1) (b 2) (g 3) (a 4) (d 5) (h 6) (e 7) (c 8))
		       ((c 1) (g 2) (b 3) (h 4) (f 5) (d 6) (a 7) (e 8))
		       ((f 1) (b 2) (g 3) (a 4) (c 5) (e 6) (h 7) (d 8))
		       ((c 1) (h 2) (d 3) (g 4) (a 5) (f 6) (b 7) (e 8))
		       ((f 1) (a 2) (e 3) (b 4) (h 5) (c 6) (g 7) (d 8))
		       ((d 1) (a 2) (e 3) (h 4) (b 5) (g 6) (c 7) (f 8))
		       ((e 1) (h 2) (d 3) (a 4) (g 5) (b 6) (f 7) (c 8))
		       ((d 1) (a 2) (e 3) (h 4) (f 5) (c 6) (g 7) (b 8))
		       ((e 1) (h 2) (d 3) (a 4) (c 5) (f 6) (b 7) (g 8))
		       ((d 1) (b 2) (e 3) (h 4) (f 5) (a 6) (c 7) (g 8))
		       ((e 1) (g 2) (d 3) (a 4) (c 5) (h 6) (f 7) (b 8))
		       ((d 1) (b 2) (g 3) (c 4) (f 5) (h 6) (a 7) (e 8))
		       ((e 1) (g 2) (b 3) (f 4) (c 5) (a 6) (h 7) (d 8))
		       ((d 1) (b 2) (g 3) (c 4) (f 5) (h 6) (e 7) (a 8))
		       ((e 1) (g 2) (b 3) (f 4) (c 5) (a 6) (d 7) (h 8))
		       ((d 1) (b 2) (g 3) (e 4) (a 5) (h 6) (f 7) (c 8))
		       ((e 1) (g 2) (b 3) (d 4) (h 5) (a 6) (c 7) (f 8))
		       ((d 1) (b 2) (h 3) (e 4) (g 5) (a 6) (c 7) (f 8))
		       ((e 1) (g 2) (a 3) (d 4) (b 5) (h 6) (f 7) (c 8))
		       ((d 1) (b 2) (h 3) (f 4) (a 5) (c 6) (e 7) (g 8))
		       ((e 1) (g 2) (a 3) (c 4) (h 5) (f 6) (d 7) (b 8))
		       ((d 1) (f 2) (a 3) (e 4) (b 5) (h 6) (c 7) (g 8))
		       ((e 1) (c 2) (h 3) (d 4) (g 5) (a 6) (f 7) (b 8))
		       ((d 1) (f 2) (h 3) (b 4) (g 5) (a 6) (c 7) (e 8))
		       ((e 1) (c 2) (a 3) (g 4) (b 5) (h 6) (f 7) (d 8))
		       ((d 1) (f 2) (h 3) (c 4) (a 5) (g 6) (e 7) (b 8))
		       ((e 1) (c 2) (a 3) (f 4) (h 5) (b 6) (d 7) (g 8))
		       ((d 1) (g 2) (a 3) (h 4) (e 5) (b 6) (f 7) (c 8))
		       ((e 1) (b 2) (h 3) (a 4) (d 5) (g 6) (c 7) (f 8))
		       ((d 1) (g 2) (c 3) (h 4) (b 5) (e 6) (a 7) (f 8))
		       ((e 1) (b 2) (f 3) (a 4) (g 5) (d 6) (h 7) (c 8))
		       ((d 1) (g 2) (e 3) (b 4) (f 5) (a 6) (c 7) (h 8))
		       ((e 1) (b 2) (d 3) (g 4) (c 5) (h 6) (f 7) (a 8))
		       ((d 1) (g 2) (e 3) (c 4) (a 5) (f 6) (h 7) (b 8))
		       ((e 1) (b 2) (d 3) (f 4) (h 5) (c 6) (a 7) (g 8))
		       ((d 1) (h 2) (a 3) (c 4) (f 5) (b 6) (g 7) (e 8))
		       ((e 1) (a 2) (h 3) (f 4) (c 5) (g 6) (b 7) (d 8))
		       ((d 1) (h 2) (a 3) (e 4) (g 5) (b 6) (f 7) (c 8))
		       ((e 1) (a 2) (h 3) (d 4) (b 5) (g 6) (c 7) (f 8))
		       ((d 1) (h 2) (e 3) (c 4) (a 5) (g 6) (b 7) (f 8))
		       ((e 1) (a 2) (d 3) (f 4) (h 5) (b 6) (g 7) (c 8))))))
