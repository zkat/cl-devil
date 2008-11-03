
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

;;; XXX I don't like the potential confusion between WITH-BOUND-IMAGE
;;; and WITH-IMAGES, but WITH-NEW-IMAGES and WITH-LOADED-IMAGES all
;;; give the wrong impression, alas.
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
                       collect `(,var (anaphora:aprog1 (cffi:mem-aref ,ids :uint ,i)
                                        ,@(w-i-args-helper args)))))
              ,@body)
         (il:delete-images ,count ,ids)))))

(defmacro with-init (&body body)
  `(progn (init)
          (unwind-protect (progn ,@body)
            (shutdown))))

(defun width-of (id)
  (bind-image id)
  (get-integer :image-width))

(defun height-of (id)
  (bind-image id)
  (get-integer :image-height))


(defun gen-images (n)
  (with-foreign-object (ids :uint n)
    (%gen-images n ids)
    (loop for i to n collect (mem-aref ids :uint i))))

(defmacro with-bound-image (id &body body)
  "Binds ID for the duration of BODY, returning to the previously bound image thereafter."
  (let ((old-image (gensym)))
    `(let ((,old-image (il:get-integer :cur-image)))
       (il:bind-image ,id)
       (unwind-protect (progn ,@body)
         (il:bind-image ,old-image)))))