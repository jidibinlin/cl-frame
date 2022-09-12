;;;; package.lisp

(defpackage #:cl-frame
  (:use #:cl)
  (:export #:length-field-based-frame-codec
           #:line-based-frame-codec
           big-endian
           little-endian
           #:encoder-config
           #:make-encoder-config
           #:decoder-config
           #:make-decoder-config
           #:read-frame
           #:write-frame
           #:put-uint16
           #:put-uint32
           #:put-uint24
           #:put-uint64
           ))
