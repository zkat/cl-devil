;;;; -*- Lisp -*-
;;;; cl-devil -- DevIL binding for CL.  See README for licensing information.

(defpackage #:cl-devil-system (:use #:cl #:asdf))
(in-package #:cl-devil-system)

(defsystem cl-devil
    :depends-on (:cffi :anaphora)
    :components
    ((:file "package")
     (:file "il" :depends-on ("package"))
     (:file "ilu" :depends-on ("package" "il"))
     (:file "ilut" :depends-on ("package" "il"))
     (:file "utilities" :depends-on ("package" "il"))))

