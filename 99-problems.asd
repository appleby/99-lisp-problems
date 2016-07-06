(defpackage #:99-system (:use :cl :asdf))
(in-package #:99-system)

(defun files-matching-pattern (pattern &key depends excludes)
  (loop for path in (directory pattern)
     unless (member (pathname-name path) excludes :test #'string=)
     collect (list :file (pathname-name path)
		   :depends-on (remove (pathname-name path) depends
				       :test #'string=))))

(defsystem #:99-problems
  :description "Solutions to the 99-Lisp problems."
  :author "Mike Appleby"
  :license "Nobody cares."
  :depends-on (#:alexandria
	       #:cl-heap
	       #:lisp-unit
	       #:split-sequence
	       #-(or sbcl clisp) #:com.informatimago.rdp)
  :components #.(files-matching-pattern
		 "p*.lisp"
		 :depends '("package")
		 ;; TODO: check for updates to com.informatimago.rdp.
		 ;; P69 and P70 depend on com.informatimago.rdp, which
		 ;; is having problems with the current versions of
		 ;; clisp and sbcl. Temporarily disable for those lisps.
		 :excludes
		 #-(or sbcl clisp) nil
		 #+(or sbcl clisp) '("p69" "p70")))
