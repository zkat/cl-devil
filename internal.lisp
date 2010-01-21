(in-package :%il)

(defun error-condition (keyword)
  (make-condition (find-symbol (symbol-name keyword) (find-package :il))))

(defmacro maybe-error (call)
  `(if ,call
       (values)
       (cl:error (error-condition (il::get-error)))))

(defmacro deferrwrap (name &optional args)
  `(defun ,name ,args
     (maybe-error (,(symbolicate "%" (symbol-name name)) ,@args))))
