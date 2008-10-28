
(in-package :cl-devil)

(defun w-i-args-helper (args)
  (when args
    `((bind-image it)
      ,(cons
        (cond ((= 1 (length args)) 'il:load-image)
              ((= 3 (length args)) 'il:load-l)
              ((stringp (second args)) 'il:load)
              (t 'il:load-f))
        args))))

(defmacro with-images ((&rest images) &body body)
  "Generates an IL image for each of IMAGES, binding and loading if a parameter is supplied.  BODY is executed, and the images are freed thereafter."
  (let ((ids (gensym))
        (count (length images)))
    `(cffi:with-foreign-object (,ids :uint ,count)
       (il:gen-images ,count ,ids)
       (unwind-protect
            (let (,@(loop for x in images
                       for (var . args) = (if (listp x) x (list x))
                       for i from 0
                       collect `(,var (anaphora:aprog1 (cffi:mem-ref ,ids :uint ,i)
                                        ,@(w-i-args-helper args)))))
              ,@body)
         (il:delete-images ,count ,ids)))))

(il:with-images (urp arp (exit "/home/julian/exit.pcx")) (format t "~&~A ~A ~A" urp arp exit))