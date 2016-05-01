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
  :depends-on (#:alexandria #:cl-heap #:com.informatimago.rdp #:lisp-unit #:split-sequence)
  :components #.(files-matching-pattern "p*.lisp" :depends '("package")))
