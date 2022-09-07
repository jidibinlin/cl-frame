;;;; cl-frame.lisp

(in-package #:cl-frame)

(defvar big-endian (make-bigendian))
(defvar little-endian (make-littlendian))


(defstruct (encoder-config (:conc-name enc-))
  ;; config for encoder
  (byte-order big-endian :type (or bigendian littlendian)) ;; byte order of the message
  (length-field-length 0 :type integer) ;; length of the length field to hold the length of the message
  (length-adjustment 0 :type integer)   ;; compensation for the msg length
  (length-includes-length-field-length t :type boolean) ;; whether include the length field length in the msg length
  )

(defstruct (decoder-config (:conc-name dec-))
  ;; config for decoder
  (byte-order big-endian :type (or bigendian littlendian)) ;; byte order of the message
  (length-field-offset 0 :type integer) ;; begin position of the length field
  (length-field-length 0 :type integer) ;; length of the length field to hold the length of the message
  (length-adjustment 0 :type integer)   ;; compensation for the msg length
  (initial-bytes-to-strip 0 :type integer) ;; initial bytes to strip from the message
  )

(defclass length-field-based-frame-codec()
  ((encoder-config
    :initarg :encoder-config
    :accessor encoder-config
    :initform nil
    :type encoder-config)
   (decoder-config
    :initarg :decoder-config
    :accessor decoder-config
    :initform nil
    :type decoder-config)
   (iostream
    :initarg :iostream
    :accessor iostream
    :initform nil
    :type stream
    )))

(defmethod read-sequence-atlist (sequence stream &key start end)
  (when (or (< (length sequence) start)
            (< (length sequence) end)
            (minusp start)
            (minusp end)
            (minusp (- end start)))
    type-error)
  )

(defmethod read-frame ((codec length-field-based-frame-codec))
  ;;read date from stream stored in the `codec' and decode it againest the codec
  ;;`CLOSED-STREAM-ERROR' will be thrown if the stream is closed, you should handler this err by yourself
  (let* ((decoder-config (decoder-config codec))
         (byte-order (dec-length-field-length decoder-config))
         (lf-length (dec-length-field-length decoder-config))
         (lf-adjustment (dec-length-adjustment decoder-config))
         (initial-bytes-to-strip (dec-initial-bytes-to-strip decoder-config))
         )

    )
  )

(defmethod write-frame ((codec length-field-based-frame-codec) buf)
  ;; encode the buf with length header and write to the socket stream stored in the `codec'
  ;; `CLOSED-STREAM-ERROR' will be thrown if the stream is closed, you should handler this err by yourself
  (let* ((encoder-config (encoder-config codec))
         ;; order of the byte
         (byte-order (enc-byte-order encoder-config))
         ;; length of the length field
         (lf-length (enc-length-field-length encoder-config))
         ;; length to be add to the length field
         (lf-adjustment (enc-length-adjustment encoder-config))
         ;; length includes the length field length
         (length (+ (length buf) lf-adjustment))
         (length-buf)
         )
	(when (enc-length-includes-length-field-length encoder-config)
      (setf length (+ length lf-length)))

    (when (<= length 0)
      (return-from write-frame (values nil (error "errTooLessLength"))))
    (case lf-length
          (1 (progn
               (when (>= length 256)
                 (error "length does not fit into one byte"))
               (setf length-buf (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0))
               (setf (aref length-buf 0) (aref (bit-smasher:octets<- length) 0))))
          (2 (progn
               (when (>= length 65536)
                 (error "length does not fit into a short integer"))
               (setf length-buf (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0))
               (put-uint16 byter-order length-buf length)))
          (3 (progn
               (when (>= length 16777216)
                 (error "length does not fit into a medium integer"))
               (setf length-buf (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0))
               (put-uint24 byte-order length-buf length)))
          (4 (progn
               (when (>= length 4294967296)
                 (error "length does not fit into a long integer"))
               (setf length-buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0))
               (put-uint32 byte-order length-buf length)))
          (8 (progn
               (when (>= length 18446744073709551616)
                 (error "length does not fit into a long long integer"))
               (setf length-buf (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
               (put-uint64 byte-order length-buf length)))
          )
    (write-sequence (concatenate 'vector length-buf buf) (iostream codec))
    (force-output (iostream codec))
    )
  )
