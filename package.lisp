
(defpackage #:cl-devil
  (:nicknames #:il)
  (:use #:cl #:cffi #:anaphora #:alexandria)
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
   #:ENABLEDP
   #:FLIP-IMAGE
   #:GEN-IMAGES
   #:GEN-IMAGE
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

(defpackage #:ilu
  (:use #:cl #:cffi)
  (:export
   :init
   :alienify
   :blur-avg
   :blur-gaussian
   :build-mipmaps
   :colours-used
   :colors-used
   :compare-image
   :contrast
   :crop
   :delete-image
   :edge-detect-p
   :edge-detect-s
   :emboss
   :enlarge-canvas
   :error-string
   :flip-image
   :gamma-correct-inter
   :gamma-correct-scale
   :gen-image
   :get-integer
   :invert-alpha
   :mirror
   :negative
   :noisify
   :pixelsize
   :rotate
   :saturate
   :scale
   :sharpen
   :swap-colours
   :swap-colors
   ;; :get-image-info
   ;; :get-string
   ;; :image-parameter
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

