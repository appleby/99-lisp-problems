;;;; (***) Conversions
;;;;
;;;; Write functions to convert between the different graph
;;;; representations. With these functions, all representations are
;;;; equivalent; i.e. for the following problems you can always pick
;;;; freely the most convenient form. The reason this problem is rated
;;;; (***) is not because it's particularly difficult, but because
;;;; it's a lot of work to deal with all the special cases.
(in-package :99)

(defun adj<-gef (gef)
  gef)

(defun adj<-digef (digef)
  digef)

(defun adj<-diadj (diadj)
  diadj)

(defun adj<-lagef (lagef)
  lagef)

(defun adj<-laadj (laadj)
  laadj)

(defun gef<-adj (adj)
  adj)

(defun gef<-digef (digef)
  digef)

(defun gef<-diadj (diadj)
  diadj)

(defun gef<-lagef (lagef)
  lagef)

(defun gef<-laadj (laadj)
  laadj)

(defun digef<-adj (adj)
  adj)

(defun digef<-gef (gef)
  gef)

(defun digef<-diadj (diadj)
  diadj)

(defun digef<-lagef (lagef)
  lagef)

(defun digef<-laadj (laadj)
  laadj)

(defun diadj<-adj (adj)
  adj)

(defun diadj<-gef (gef)
  gef)

(defun diadj<-digef (digef)
  digef)

(defun diadj<-lagef (lagef)
  lagef)

(defun diadj<-laadj (laadj)
  laadj)

(defun lagef<-adj (adj)
  adj)

(defun lagef<-gef (gef)
  gef)

(defun lagef<-digef (digef)
  digef)

(defun lagef<-diadj (diadj)
  diadj)

(defun lagef<-laadj (laadj)
  laadj)

(defun laadj<-adj (adj)
  adj)

(defun laadj<-gef (gef)
  gef)

(defun laadj<-digef (digef)
  digef)

(defun laadj<-diadj (diadj)
  diadj)

(defun laadj<-lagef (lagef)
  lagef)

(define-test graph-to-*-test
    (let ((gef '((b c d f g h k) ((b c) (b f) (c f) (f k) (g h))))
	  (adj '((b (c f)) (c (b f)) (d ()) (f (b c k)) (g (h)) (h (g)) (k (f))))
	  (digef '((b c d f g h k) ((b c) (c b) (b f) (f b) (c f) (f c) (f k) (k f) (g h) (h g))))
	  (diadj '((b (c f)) (c (b f)) (d ()) (f (b c k)) (g (h)) (h (g)) (k (f))))
	  (lagef '((b c d f g h k) ((b c 1) (b f 1) (c f 1) (f k 1) (g h 1))))
	  (laadj '((b ((c 1) (f 1))) (c ((b 1) (f 1))) (d (())) (f ((b 1) (c 1) (k 1))) (g ((h 1))) (h ((g 1))) (k ((f 1))))))
      (assert-equality #'tree-equal gef (gef<-adj (adj<-gef gef)))
      (assert-equality #'tree-equal gef (gef<-digef (digef<-gef gef)))
      (assert-equality #'tree-equal gef (gef<-diadj (diadj<-gef gef)))
      (assert-equality #'tree-equal gef (gef<-lagef (lagef<-gef gef)))
      (assert-equality #'tree-equal gef (gef<-laadj (laadj<-gef gef)))
      (assert-equality #'tree-equal adj (adj<-gef (gef<-adj adj)))
      (assert-equality #'tree-equal adj (adj<-digef (digef<-adj adj)))
      (assert-equality #'tree-equal adj (adj<-diadj (diadj<-adj adj)))
      (assert-equality #'tree-equal adj (adj<-lagef (lagef<-adj adj)))
      (assert-equality #'tree-equal adj (adj<-laadj (laadj<-adj adj)))
      (assert-equality #'tree-equal digef (digef<-adj (adj<-digef digef)))
      (assert-equality #'tree-equal digef (digef<-gef (gef<-digef digef)))
      (assert-equality #'tree-equal digef (digef<-diadj (diadj<-digef digef)))
      (assert-equality #'tree-equal digef (digef<-lagef (lagef<-digef digef)))
      (assert-equality #'tree-equal digef (digef<-laadj (laadj<-digef digef)))
      (assert-equality #'tree-equal diadj (diadj<-adj (adj<-diadj diadj)))
      (assert-equality #'tree-equal diadj (diadj<-gef (gef<-diadj diadj)))
      (assert-equality #'tree-equal diadj (diadj<-digef (digef<-diadj diadj)))
      (assert-equality #'tree-equal diadj (diadj<-lagef (lagef<-diadj diadj)))
      (assert-equality #'tree-equal diadj (diadj<-laadj (laadj<-diadj diadj)))
      (assert-equality #'tree-equal lagef (lagef<-adj (adj<-lagef lagef)))
      (assert-equality #'tree-equal lagef (lagef<-gef (gef<-lagef lagef)))
      (assert-equality #'tree-equal lagef (lagef<-diadj (diadj<-lagef lagef)))
      (assert-equality #'tree-equal lagef (lagef<-digef (digef<-lagef lagef)))
      (assert-equality #'tree-equal lagef (lagef<-laadj (laadj<-lagef lagef)))
      (assert-equality #'tree-equal laadj (laadj<-adj (adj<-laadj laadj)))
      (assert-equality #'tree-equal laadj (laadj<-gef (gef<-laadj laadj)))
      (assert-equality #'tree-equal laadj (laadj<-digef (digef<-laadj laadj)))
      (assert-equality #'tree-equal laadj (laadj<-diadj (diadj<-laadj laadj)))
      (assert-equality #'tree-equal laadj (laadj<-lagef (lagef<-laadj laadj)))))
