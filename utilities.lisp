(in-package :cl-devil)

;;; XXX I don't like the potential confusion between WITH-BOUND-IMAGE
;;; and WITH-IMAGES, but WITH-NEW-IMAGES and WITH-LOADED-IMAGES all
;;; give the wrong impression, alas.
(defmacro with-images ((&rest images) &body body)
  "Generates an IL image for each of IMAGES, binding and loading if a parameter is supplied.  BODY is executed, and the images are freed thereafter."
  (flet ((args-helper (args)
           (when args
             `((bind-image it)
               ,(cons
                 (cond ((= 1 (length args)) 'il:load-image)
                       ((= 3 (length args)) 'il:load-l)
                       ((stringp (second args)) 'il:load)
                       (t 'il:load-f))
                 args)))))
   (let ((ids (gensym "IDS"))
         (count (length images)))
     `(cffi:with-foreign-object (,ids :uint ,count)
        (%gen-images ,count ,ids)
        (unwind-protect
             (let (,@(loop for x in images
                        for (var . args) = (if (listp x) x (list x))
                        for i from 0
                        collect `(,var (let ((it (cffi:mem-aref ,ids :uint ,i)))
                                         ,@(args-helper args)
                                         it))))
               ,@body)
          (%delete-images ,count ,ids))))))

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

(defun pixel-format-of (id)
  (bind-image id)
  (foreign-enum-keyword 'data-format (get-integer :image-format)))

(defun element-type-of (id)
  (bind-image id)
  (foreign-enum-keyword 'data-type (get-integer :image-type)))

(defun bytes-per-pixel-of (id)
  (bind-image id)
  (get-integer :image-bytes-per-pixel))

(defun copy-palette (dest src)
  (bind-image src)
  (let ((type (get-integer :palette-type))
	(ncols (get-integer :palette-num-cols))
	(bpp (get-integer :palette-bpp))
	(pointer (get-palette)))
    (bind-image dest)
    (register-palette pointer (* ncols bpp) type)))

; The standard C lib is loaded by default. I'm not entirely sure where to put
; the following:
(defcfun ("memset" memset) :pointer
  (dest :pointer) (value :int) (size :unsigned-int))

(defcfun ("memcpy" memcpy) :pointer
  (dest :pointer) (src :pointer) (size :unsigned-int))

(defun clear-image (&rest channel-values)
  "Clears the bound image by setting the channel values for all pixels."
  (let* ((iterator (get-data))
         (data iterator)
         (bytes-per-pixel (get-integer :image-bytes-per-pixel))
         (width (get-integer :image-width))
         (height (get-integer :image-height))
         (data-type (ecase (foreign-enum-keyword 
                            'data-type (get-integer :image-type))
                      (:byte :int8)
                      (:double :double)
                      (:float :float)
                      (:int :int)
                      (:short :short)
                      (:unsigned-int :unsigned-int)
                      (:unsigned-byte :uint8)
                      (:unsigned-short :unsigned-short)))
         (data-type-size (foreign-type-size data-type))
         (row-size (* width bytes-per-pixel)))
    (assert (= (* data-type-size (length channel-values)) bytes-per-pixel) ()
            "incorrect channel count")
    (if (apply #'= (cons 0 channel-values))
        (memset data 0 (* width height bytes-per-pixel))
        (with-foreign-object (src-pixel :uint8 bytes-per-pixel)
          (let ((iterator src-pixel))
            (dolist (channel channel-values)
              (setf (mem-aref iterator data-type)
                    channel)
              (incf-pointer iterator data-type-size)))
          (dotimes (i width)
            (memcpy iterator src-pixel bytes-per-pixel)
            (incf-pointer iterator bytes-per-pixel))
          (dotimes (i (1- height)) 
            (memcpy iterator data row-size)
            (incf-pointer iterator row-size))))))

(defmacro with-bound-image (id &body body)
  "Binds ID for the duration of BODY, returning to the previously bound image thereafter."
  (let ((old-image (gensym)))
    `(let ((,old-image (il:get-integer :cur-image)))
       (il:bind-image ,id)
       (unwind-protect (progn ,@body)
         (il:bind-image ,old-image)))))