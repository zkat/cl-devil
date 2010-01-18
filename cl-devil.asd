;;;; -*- Lisp -*-
;;;; cl-devil -- DevIL binding for CL.  See README for licensing information.

(asdf:defsystem cl-devil
  :depends-on (:cffi :anaphora :alexandria)
  :components
  ((:file "package")
   (:file "il" :depends-on ("package"))
   (:file "ilu" :depends-on ("package" "il"))
   (:file "ilut" :depends-on ("package" "il"))
   (:file "utilities" :depends-on ("package" "il"))))

