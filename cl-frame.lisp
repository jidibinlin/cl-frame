;;;; cl-frame.lisp

(in-package #:cl-frame)

(defvar big-endian (make-bigendian))
(defvar little-endian (make-littlendian))


(defstruct (encoder-config (:conc-name enc-))
  ;; config for encoder
  (byte-order big-endian :type (or bigendian littlendian)) ;; byte order of the message
  (length-field-length 0 :type integer) ;; length of the length field to hold the length of the message
  (length-field-offset 0 :type integer) ;; length field start position
  (length-adjustment 0 :type integer)   ;; compensation for the msg length
  (length-includes-length-field-length nil :type boolean) ;; whether include the length field length in the msg length
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

;; (defmethod read-sequence-atlist (sequence stream &key start end)
;;   (let ((position (read-sequence sequence stream :start start :end end)))
;;     (when (< position end)
;;       (setf start position)
;;       (read-sequence-atlist sequence stream :start position :end end)))
;;   )

(defmethod read-frame ((codec length-field-based-frame-codec))
  ;;read date from stream stored in the `codec' and decode it againest the codec
  ;;`CLOSED-STREAM-ERROR' will be thrown if the stream is closed, you should handler this err by yourself
  (let* ((byte-order (dec-length-field-length (decoder-config codec)))
         (lf-offset (dec-length-field-offset (decoder-config codec)))
         (lf-length (dec-length-field-length (decoder-config codec)))
         (lf-adjustment (dec-length-adjustment (decoder-config codec)))
         (initial-bytes-to-strip (dec-initial-bytes-to-strip (decoder-config codec)))
         (header (make-array 0 :element-type '(unsigned-byte 8)))
         (msg))
    (when (> lf-offset 0)
      (setf header (make-array lf-offset :element-type '(unsigned-byte 8)))
      (read-sequence header (iostream codec) :start 0 :end lf-offset)
      )
    (multiple-value-bind (lenbuf msg-len) (get-unadjusted-frame-length codec)
      (setf msg-len (+ msg-len lf-adjustment))
      (setf msg (make-array msg-len :element-type '(unsigned-byte 8) :initial-element 0))
      (read-sequence msg (iostream codec) :start 0 :end msg-len)

      (subseq (concatenate 'vector header lenbuf msg) initial-bytes-to-strip)))
  )

(defmethod get-unadjusted-frame-length ((codec length-field-based-frame-codec))
  (let* ((dec-config (decoder-config codec))
         (len-buf)
         (length))
    (case (dec-length-field-length dec-config)
      (1 (progn
           (setf len-buf (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0))
           (read-sequence len-buf (iostream codec) :start 0 :end 1)
           (setf length (bit-smasher:int<- len-buf)))
       (values len-buf length))

      (2 (progn
           (setf len-buf (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0))
           (read-sequence len-buf (iostream codec) :start 0 :end 2)
           (setf length (bit-smasher:int<- len-buf)))
       (values len-buf length))

      (3 (progn
           (setf len-buf (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0))
           (read-sequence len-buf (iostream codec) :start 0 :end 3)
           (setf length (bit-smasher:int<- len-buf)))
       (values len-buf length))

      (4 (progn
           (setf len-buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0))
           (read-sequence len-buf (iostream codec) :start 0 :end 4)
           (setf length (bit-smasher:int<- len-buf)))
       (values len-buf length))

      (8 (progn
           (setf len-buf (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
           (read-sequence len-buf (iostream codec) :start 0 :end 8)
           (setf length (bit-smasher:int<- len-buf)))
       (values len-buf length))
      )
    )
  )


(define-condition too-large-length-error (error) ())
(define-condition minus-length-adjustment-too-small-error (error) ())
(define-condition length-field-offset-mismatch-header (error) ())

(defmethod write-frame ((codec length-field-based-frame-codec) buf &key header)
  ;; encode the buf with length header and write to the socket stream stored in the `codec'
  ;; if `header' is not nil header will be prepeneded to the buf which composed by length header and `buf'
  ;; `CLOSED-STREAM-ERROR' will be thrown if the stream is closed, you should handler this err by yourself
  ;; `TOO-LARGE-LENGTH' will be thrown if width of  length-field can`t hold the length
  ;; `MINUS-LENGTH-ADJUSTMENT-TOO-SMALL-ERROR' will be thrown if minused length-adjustment make the encoded buf length minus
  (let* (;; order of the byte
         (byte-order (enc-byte-order (encoder-config codec)))
         ;; length of the length field
         (lf-length (enc-length-field-length (encoder-config codec)))
         ;; length to be add to the length field
         (lf-adjustment (enc-length-adjustment (encoder-config codec)))
         ;; where length field start
         (lf-offset (enc-length-field-offset (encoder-config codec)))
         ;; length includes the length field length
         (length (+ (length buf) lf-adjustment))
         (length-buf)
         )
	(when (not (equal nil header))
      (when (> (length header)
               (1- (expt 2 lf-offset)))
        (error 'length-field-offset-mismatch-header)
        )
      )

	(when (enc-length-includes-length-field-length (encoder-config codec))
      (setf length (+ length lf-length)))

    (when (<= length 0)
      (error 'minus-length-adjustment-too-small))
    (case lf-length
      (1 (progn
           (when (>= length 256)
             (error 'too-large-length))
           (setf length-buf (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0))
           (setf (aref length-buf 0) (aref (bit-smasher:octets<- length) 0))))
      (2 (progn
           (when (>= length 65536)
             (error 'too-large-length))
           (setf length-buf (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0))
           (put-uint16 byte-order length-buf length)))
      (3 (progn
           (when (>= length 16777216)
             (error 'too-large-length))
           (setf length-buf (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0))
           (put-uint24 byte-order length-buf length)))
      (4 (progn
           (when (>= length 4294967296)
             (error 'too-large-length))
           (setf length-buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0))
           (put-uint32 byte-order length-buf length)))
      (8 (progn
           (when (>= length 18446744073709551616)
             (error 'too-large-length))
           (setf length-buf (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
           (put-uint64 byte-order length-buf length)))
      )
    (write-sequence (concatenate 'vector header length-buf buf) (iostream codec))
    (force-output (iostream codec))))
