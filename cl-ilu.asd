;;;; -*- Lisp -*-
;;;; cl-ilu -- libILU binding for CL.  See README for licensing information.

(asdf:defsystem cl-ilu
  :depends-on (#:cffi #:alexandria #:cl-devil)
  :components
  ((:file "ilu")))
