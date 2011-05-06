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

(defmacro with-bound-image (id &body body)
  "Binds ID for the duration of BODY, returning to the previously bound image thereafter."
  (let ((old-image (gensym)) (body-func (gensym)))
    `(flet ((,body-func () ,@body))
       (if (eq ,id :current-image)
         (,body-func)
         (let ((,old-image (il:get-integer :cur-image)))
           (il:bind-image ,id)
           (unwind-protect (,body-func)
             (il:bind-image ,old-image)))))))

(defun image-width (&optional (id :current-image))
  (with-bound-image id
    (get-integer :image-width)))

(defun image-height (&optional (id :current-image))
  (with-bound-image id
    (get-integer :image-height)))

(defun image-format (&optional (id :current-image))
  (with-bound-image id
    (foreign-enum-keyword 'data-format (get-integer :image-format))))

(defun image-type (&optional (id :current-image))
  (with-bound-image id
    (foreign-enum-keyword 'data-type (get-integer :image-type))))

(defun image-bytes-per-pixel (&optional (id :current-image))
  (with-bound-image id
    (get-integer :image-bytes-per-pixel)))

(defun image-bits-per-pixel (&optional (id :current-image))
  (with-bound-image id
    (get-integer :image-bits-per-pixel)))

(defun image-origin (&optional (id :current-image))
  (with-bound-image id
    (foreign-enum-keyword 'origin (get-integer :image-origin))))

(defmacro define-deprecated-image-fun (deprecated new)
  `(progn (declaim (inline ,deprecated))
          (defun ,deprecated (id)
            (,new id))
          (define-compiler-macro ,deprecated (id)
            (warn "~A is deprecated. Please use ~A instead." ',deprecated ',new)
            (list ',new id))))

(define-deprecated-image-fun width-of image-width)
(define-deprecated-image-fun height-of image-height)
(define-deprecated-image-fun pixel-format-of image-format)
(define-deprecated-image-fun element-type-of image-type)
(define-deprecated-image-fun bytes-per-pixel-of image-bytes-per-pixel)

(defun copy-palette (dest src)
  (bind-image src)
  (let ((type (get-integer :palette-type))
        (ncols (get-integer :palette-num-cols))
        (bpp (get-integer :palette-bpp))
        (pointer (get-palette)))
    (bind-image dest)
    (register-palette pointer (* ncols bpp) type)))
