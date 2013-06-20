;;;; -*- Lisp -*-
;;;; cl-ilut -- libILUT binding for CL.  See README for licensing information.

(asdf:defsystem cl-ilut
  :depends-on (#:cffi #:alexandria #:cl-devil)
  :components
  ((:file "ilut")))
