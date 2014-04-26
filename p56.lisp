;;;; (**) Symmetric binary trees
;;;;
;;;; Let us call a binary tree symmetric if you can draw a vertical
;;;; line through the root node and then the right subtree is the
;;;; mirror image of the left subtree. Write a function symmetric to
;;;; check whether a given binary tree is symmetric. We are only
;;;; interested in the structure, not in the contents of the nodes.

(in-package :99)

(defun p56-symmetric (tree)
  (labels ((mirror-images-p (t1 t2)
	     (cond ((and (null t1) (null t2)) t)
		   ((or (null t1) (null t2)) nil)
		   (t (and (mirror-images-p (cadr t1) (caddr t2))
			   (mirror-images-p (caddr t1) (cadr t2)))))))
    (or (null tree)
	(mirror-images-p (cadr tree) (caddr tree)))))

(define-test p56-symmetric-test
  (let ((symmetric-trees '(nil
			   (x nil nil)
			   (x (x nil nil) (x nil nil))
			   (x (x (x nil nil) nil) (x nil (x nil nil)))
			   (x (x nil (x nil nil)) (x (x nil nil) nil))
			   (x (x (x (x nil nil) nil) nil) (x nil (x nil (x nil nil))))
			   (x (x nil (x (x nil nil) nil)) (x (x nil (x nil nil)) nil))))
	(asymmetric-trees '((x (x nil nil) nil)
			    (x nil (x nil nil))
			    (x (x (x nil nil) nil) (x (x nil nil) nil))
			    (x (x nil (x nil nil)) (x nil (x nil nil))))))
    (loop for tree in symmetric-trees do (assert-true (p56-symmetric tree) tree))
    (loop for atree in asymmetric-trees do (assert-false (p56-symmetric atree)))))
