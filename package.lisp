
(defpackage #:cl-devil
  (:nicknames #:il)
  (:use #:cl #:cffi :anaphora)
  (:shadow #:load #:error)
  (:export
   #:with-bound-image
   #:with-images
   #:with-init
   #:width-of
   #:height-of
   #:pixel-format-of
   #:element-type-of
   #:bytes-per-pixel-of
   #:copy-palette
   ;; bindings
   #:BIND-IMAGE
   #:BLIT
   #:CONVERT-IMAGE
   #:COPY-IMAGE
   #:COPY-PIXELS
   #:DELETE-IMAGES
   #:DETERMINE-TYPE
   #:DISABLE
   #:ENABLE
   #:ENABLED-P
   #:FLIP-IMAGE
   #:GEN-IMAGES
   #:GET-DATA
   #:GET-ERROR
   #:GET-INTEGER
   #:GET-PALETTE
   #:REGISTER-PALETTE
   #:INIT
   #:IS-ENABLED
   #:KEY-COLOR
   #:LOAD
   #:LOAD-F
   #:LOAD-IMAGE
   #:LOAD-L
   #:OVERLAY-IMAGE
   #:SAVE
   #:SAVE-F
   #:SAVE-IMAGE
   #:SAVE-L
   #:SET-DATA
   #:SET-INTEGER
   #:SET-PIXELS
   #:SHUTDOWN
   #:TEX-IMAGE
   ))

(defpackage #:ilut
  (:use #:cl #:cffi)
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
