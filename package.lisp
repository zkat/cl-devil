(defpackage #:%cl-devil-internal
  (:nicknames #:%il)
  (:use #:cl #:alexandria)
  (:shadow #:error)
  (:export
   #:error-condition
   #:maybe-error
   #:deferrwrap))

(defpackage #:cl-devil
  (:nicknames #:il)
  (:use #:cl #:cffi #:%il)
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
   #:image-width
   #:image-height
   #:image-format
   #:image-type
   #:image-bytes-per-pixel
   #:image-bits-per-pixel
   #:image-origin
   #:copy-palette
   #:clear-image
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
   ;; Errors
   #:error
   #:check-error
   #:no-error
   #:invalid-enum
   #:out-of-memory
   #:format-not-supported
   #:internal-error
   #:invalid-value
   #:illegal-operation
   #:illegal-file-value
   #:invalid-file-header
   #:invalid-param
   #:could-not-open-file
   #:invalid-extension
   #:file-already-exists
   #:out-format-same
   #:stack-overflow
   #:stack-underflow
   #:invalid-conversion
   #:bad-dimensions
   #:file-read-error
   #:file-write-error
   #:lib-gif-error
   #:lib-jpeg-error
   #:lib-png-error
   #:lib-tiff-error
   #:lib-mng-error
   #:unknown-error
   ))

(defpackage #:ilu
  (:use #:cl #:cffi #:%il)
  (:shadow #:error)
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

