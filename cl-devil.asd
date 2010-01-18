;;;; -*- Lisp -*-
;;;; cl-devil -- DevIL binding for CL.  See README for licensing information.

(asdf:defsystem cl-devil
  :depends-on (#:cffi #:alexandria)
  :components
  ((:file "package")
   (:file "internal" :depends-on ("package"))
   (:file "il" :depends-on ("package" "internal"))
   (:file "ilu" :depends-on ("package" "internal" "il"))
   (:file "ilut" :depends-on ("package" "internal" "il"))
   (:file "utilities" :depends-on ("package" "il"))))

