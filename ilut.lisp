;;;; cl-devil -- DevIL binding for CL.  See README for licensing information.
(defpackage #:ilut
  (:use #:cl #:cffi #:%il)
  (:shadow #:error)
  (:export
   #:CONVERT-TO-SDL-SURFACE
   #:DISABLE
   #:ENABLE
   #:GET-BOOLEAN
   #:GL-BIND-MIPMAPS
   #:GL-BIND-TEX-IMAGE
   #:GL-BUILD-MIPMAPS
   #:GL-LOAD-IMAGE
   #:GL-SAVE-IMAGE
   #:GL-SCREEN
   #:GL-SCREENIE
   #:GL-SET-TEX
   #:GL-SUB-TEX
   #:GL-TEX-IMAGE
   #:INIT
   #:RENDERER
   #:SDL-SURFACE-FROM-BITMAP
   #:SDL-SURFACE-LOAD-IMAGE
   ))

(in-package :ilut)

(define-foreign-library ilut
  (:darwin (:or "libILUT.dylib" "libILUT.1.dylib"))
  (:unix (:or "libILUT.so" "libILUT.so.1"))
  (:windows (:or "ILUT.dll" "libILUT-1.dll"))
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

(defcfun ("ilutRenderer" %renderer) :boolean (renderer renderer))
(deferrwrap renderer (renderer))
(defcfun ("ilutEnable" %enable) :boolean (state state-definition))
(deferrwrap enable (state))
(defcfun ("ilutDisable" %disable) :boolean (state state-definition))
(deferrwrap disable (state))
(defcfun ("ilutGetBoolean" %get-boolean) :boolean (state state-definition))
(deferrwrap get-boolean (state))
(defcfun ("ilutInit" %init) :boolean)
(deferrwrap init)
;;; OpenGL
(defcfun ("ilutGLBindTexImage" gl-bind-tex-image) :uint)
(defcfun ("ilutGLBindMipmaps" gl-bind-mipmaps) :uint)
(defcfun ("ilutGLBuildMipmaps" %gl-build-mipmaps) :boolean)
(deferrwrap gl-build-mipmaps)
(defcfun ("ilutGLLoadImage" gl-load-image) :uint (file-name :string))
(defcfun ("ilutGLScreen" %gl-screen) :boolean)
(deferrwrap gl-screen)
(defcfun ("ilutGLScreenie" %gl-screenie) :boolean)
(deferrwrap gl-screenie)
(defcfun ("ilutGLSaveImage" %gl-save-image) :boolean (file-name :string) (texture-id :uint))
(deferrwrap gl-save-image (file-name texture-id))
(defcfun ("ilutGLSetTex" %gl-set-tex) :boolean (texture-id :uint))
(deferrwrap gl-set-tex (texture-id))
(defcfun ("ilutGLTexImage" %gl-tex-image) :boolean (level :uint))
(deferrwrap gl-tex-image (level))
(defcfun ("ilutGLSubTex" %gl-sub-tex) :boolean (texture-id :uint) (x :uint) (y :uint))
(deferrwrap gl-sub-tex (texture-id x y))
;;; SDL
#-win32
(defcfun ("ilutConvertToSDLSurface" convert-to-sdl-surface) :pointer (flags :uint))
#-win32
(defcfun ("ilutSDLSurfaceLoadImage" sdl-surface-load-image) :pointer (file-name :string))
#-win32
(progn
  (defcfun ("ilutSDLSurfaceFromBitmap" %sdl-surface-from-bitmap) :boolean (surface :pointer))
  (deferrwrap sdl-surface-from-bitmap (surface)))
