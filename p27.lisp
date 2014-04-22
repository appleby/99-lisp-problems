;;;; (**) Group the elements of a set into disjoint subsets.
;;;;
;;;; a) In how many ways can a group of 9 people work in 3 disjoint
;;;; subgroups of 2, 3 and 4 persons? Write a function that generates
;;;; all the possibilities and returns them in a list.
;;;;
;;;; Example:
;;;; * (group3 '(aldo beat carla david evi flip gary hugo ida))
;;;; ( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
;;;; ... )
;;;;
;;;; b) Generalize the above predicate in a way that we can specify a
;;;; list of group sizes and the predicate will return a list of
;;;; groups.
;;;;
;;;; Example:
;;;; * (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
;;;; ( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
;;;; ... )
;;;;
;;;; Note that we do not want permutations of the group members;
;;;; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO)
;;;; ...). However, we make a difference between ((ALDO BEAT) (CARLA
;;;; DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).
;;;;
;;;; You may find more about this combinatorial problem in a good book
;;;; on discrete mathematics under the term "multinomial
;;;; coefficients".
(in-package :99)

(defun remove-all (items lst)
  (remove-if (lambda (item) (member item items)) lst))

(defun p27-group (names group-sizes)
  (if (or (null names) (null group-sizes))
      '(())
      (loop for combo in (p26-combination (first group-sizes) names)
	 append (loop for partial in (p27-group (remove-all combo names)
						(rest group-sizes))
		   collect (cons combo partial)))))

(define-test p27-group-known-solutions
  (let ((names '(aldo beat carla david evi flip gary hugo ida))
	(known-solutions '(((2 3 4) 1260 (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA))    ; (2, 3, 4)! = 1260
			   ((2 2 5) 756  (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA))))) ; (2, 2, 5)! = 756
    (loop for (sizes num-solutions . known-solution) in known-solutions
       do (let ((solutions (p27-group names sizes)))
	    (assert-equal num-solutions (length solutions))
	    (assert-true (member known-solution solutions :test #'equal))))))
