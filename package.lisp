(defpackage #:99-problems
  (:use #:cl #:lisp-unit #:split-sequence)
  ;; TODO: check for updates to com.informatimago.rdp.
  ;; P69 and P70 depend on com.informatimago.rdp, which
  ;; is having problems with the current versions of
  ;; clisp and sbcl. Temporarily disable for those lisps.
  #-(or sbcl clisp) (:use  #:com.informatimago.rdp)
  (:import-from #:alexandria #:with-gensyms #:once-only))
