;;;; -*- Lisp -*-
;;;; cl-devil -- DevIL binding for CL.  See README for licensing information.

(defpackage #:cl-devil-system (:use #:cl #:asdf))
(in-package #:cl-devil-system)

(defsystem cl-devil
    :depends-on (:cffi)
    :components
    ((:file "package")
     (:file "il" :depends-on ("package"))
     #+(or)(:file "ilu" :depends-on ("package" "il"))
     #+(or)(:file "ilut" :depends-on ("package" "ilu"))
     (:file "utilities" :depends-on ("package" "il"))))

