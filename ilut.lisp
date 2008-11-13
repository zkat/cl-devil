;;;; cl-devil -- DevIL binding for CL.  See README for licensing information.

(in-package :ilut)

(define-foreign-library ilut
  (:unix (:or "libILUT" "libILUT.so.1"))
  (t (:default "libILUT")))
(use-foreign-library ilut)

(defcenum state-definition
  (:palette-mode #x0600)
  (:opengl-conv #x0610)
  (:d3d-miplevels #x0620)
  (:maxtex-width #x0630)
  (:maxtex-height #x0631)
  (:maxtex-depth #x0632)
  (:gl-use-s3tc #x0634)
  (:d3d-use-dxtc #x0634)
  (:gl-gen-s3tc #x0635)
  (:d3d-gen-dxtc #x0635)
  (:s3tc-format #x0705)
  (:dxtc-format #x0705)
  (:d3d-pool #x0706)
  (:d3d-alpha-key-color #x0707)
  (:d3d-alpha-key-colour #x0707))

(defcenum renderer
  (:opengl 0)
  (:allegro 1)
  (:win32 2)
  (:direct3d8 3)
  (:direct3d9 4))

(defcfun ("ilutRenderer" renderer) :boolean (renderer renderer))
(defcfun ("ilutEnable" enable) :boolean (state state-definition))
(defcfun ("ilutDisable" disable) :boolean (state state-definition))
(defcfun ("ilutGetBoolean" get-boolean) :boolean (state state-definition))
(defcfun ("ilutInit" init) :boolean)
;;; OpenGL
(defcfun ("ilutGLBindTexImage" gl-bind-tex-image) :uint)
(defcfun ("ilutGLBindMipmaps" gl-bind-mipmaps) :uint)
(defcfun ("ilutGLBuildMipmaps" gl-build-mipmaps) :boolean)
(defcfun ("ilutGLLoadImage" gl-load-image) :uint (file-name :string))
(defcfun ("ilutGLScreen" gl-screen) :boolean)
(defcfun ("ilutGLScreenie" gl-screenie) :boolean)
(defcfun ("ilutGLSaveImage" gl-save-image) :boolean (file-name :string) (tex-id :uint))
(defcfun ("ilutGLSetTex" gl-set-tex) :boolean (tex-id :uint))
(defcfun ("ilutGLTexImage" gl-tex-image) :boolean (level :uint))
(defcfun ("ilutGLSubTex" gl-sub-tex) :boolean (tex-id :uint) (x-offset :uint) (y-offset :uint))
;;; SDL
(defcfun ("ilutConvertToSDLSurface" convert-to-sdl-surface) :pointer (flags :uint))
(defcfun ("ilutSDLSurfaceLoadImage" sdl-surface-load-image) :pointer (file-name :string))
(defcfun ("ilutSDLSurfaceFromBitmap" sdl-surface-from-bitmap) :boolean (surface :pointer))
