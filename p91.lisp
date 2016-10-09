;;; (**) Knight's tour
;;;
;;; Another famous problem is this one: How can a knight jump on an
;;; NxN chessboard in such a way that it visits every square exactly
;;; once?
;;;
;;; Hints: Represent the squares by pairs of their coordinates of the
;;; form (X Y), where both X and Y are integers between 1 and
;;; N. Define a function (jump N (X Y)) that returns a list of the
;;; positions (U V) such that a knight can jump from (X Y) to (U V) on
;;; a NxN chessboard. And finally, represent the solution of our
;;; problem as a list of N*N knight positions (the knight's tour).
(in-package :99-problems)

(defvar *possible-knight-moves* nil
  "Initialized to an NxN array of possible knight moves to/from each
square on a NxN board.")

(defun knights-tour (n)
  "Return a knight's tour of an NxN board.

Search proceeds via Warnsdorf's rule, i.e. candidate moves are ordered
by increasing degree."
  (knights-tour-heuristic n (lambda (next-moves) (sort next-moves #'warnsdorf))))

(defun knights-tour-brute-force (n)
  "Return a knight's tour of an NxN board.

Search proceeds via brute-force."
  (knights-tour-heuristic n #'identity))

(defun knights-tour-heuristic (n next-moves &optional (start (make-square 1 1)))
  "Return a knight's tour of an NxN board.

NEXT-MOVES is passed a list of all possible candidate next moves, and
should return an ordered list of next moves for the search to try."
  (assert (plusp n))
  (assert (valid-square-p n start))
  (let ((*possible-knight-moves* (make-moves-table n)))
    (labels ((rec (tour)
	       (if (= (* n n) (length tour))
		   (nreverse tour)
		   (some (lambda (jump-to) (rec (cons jump-to tour)))
			 (funcall next-moves
				  (remove-if (lambda (sq)
					       (member sq tour :test #'equal))
					     (lookup-moves (first tour))))))))
      (rec (list start)))))

(defun make-square (row column)
  "Return a chessboard square from row and column."
  (list row column))

(defun square-row (square)
  (first square))

(defun square-column (square)
  (second square))

(defun make-moves-table (n)
  "Return an NxN array of possible moves from each square on a NxN board.

The element at index (i,j) holds a list of possible moves to/from the
square at (i+1,j+1)."
  (let ((table (make-array (list n n))))
    (loop for i upto (1- n) for si from 1 upto n
       do (loop for j upto (1- n) for sj from 1 upto n
	     do (setf (aref table i j) (jump n (make-square si sj)))))
    table))

(defun lookup-moves (square)
  "Lookup the list of possible moves to/from SQUARE in
*POSSIBLE-KNIGHT-MOVES*."
  (aref *possible-knight-moves*
	(1- (square-row square))
	(1- (square-column square))))

(defun warnsdorf (sq1 sq2)
  "Return true if SQ1 is reachable from fewer squares than SQ2."
  (< (length (lookup-moves sq1)) (length (lookup-moves sq2))))

(defun jump (n square)
  "Return the list of squares reachable from SQUARE on an NxN board."
  (assert (valid-square-p n square))
  (remove-if-not (lambda (s) (valid-square-p n s))
		 (mapcar (lambda (delta)
			   (make-square (+ (square-row square) (first delta))
					(+ (square-column square) (second delta))))
			 (append (cartesian-product '(2 -2) '(1 -1))
				 (cartesian-product '(1 -1) '(2 -2))))))

(defun between-p (n low high &key (test-low #'>=) (test-high #'<=))
  "Return true if N is between LOW and HIGH."
  (and (funcall test-low n low) (funcall test-high n high)))

(defun valid-square-p (n square)
  "Return t if SQUARE is in-bounds on an NxN board."
  (and (between-p (square-row square) 1 n)
       (between-p (square-column square) 1 n)))

(defun valid-knights-tour-p (n tour)
  "Return t if TOUR is a valid knight's tour of an NxN board."
  (or (and (member n '(2 3 4))
	   (null tour))
      (and (set-equal (range 1 n) (mapcar #'square-row tour))
	   (set-equal (range 1 n) (mapcar #'square-column tour))
	   (reduce (lambda (acc sq)
		     (and acc (member sq (jump n acc) :test #'equal) sq))
		   tour))))

(define-test knights-tour-test
  (loop for n from 1 upto 8
     when (< n 6)
     do (assert-true (valid-knights-tour-p n (knights-tour-brute-force n)))
     do (assert-true (valid-knights-tour-p n (knights-tour n)))))
