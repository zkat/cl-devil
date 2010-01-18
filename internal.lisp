(in-package :%il)

(defmacro maybe-error (call)
  `(if ,call
       (values)
       (cl:error (make-condition (find-symbol (symbol-name (il::get-error)) (find-package :il))))))

(defmacro deferrwrap (name &optional args)
  `(defun ,name ,args
     (maybe-error (,(symbolicate "%" (symbol-name name)) ,@args))))
