;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-frame/test.read-write-frame
  (:use #:cl
        #:rove
        #:usocket)
  (:shadowing-import-from #:rove
                          #:*debug-on-error*))

(in-package #:cl-frame/test.read-write-frame)

(setup
  (defparameter listener (usocket:socket-listen "localhost" 1990 :element-type '(unsigned-byte 8)))
  (format t "=============================listener inited============================ ~%"))
(teardown
  (usocket:socket-close listener)
  (format t "=============================listener closed============================ ~%"))

;; (defparameter server (usocket:socket-listen "localhost" 1990 :element-type '(unsigned-byte 8)))

;; * 2 bytes length field at offset 0, do not strip header
;; lengthFieldOffset   = 0
;; lengthFieldLength   = 2
;; lengthAdjustment    = 0
;; initialBytesToStrip = 0 (= do not strip header)
;; BEFORE DECODE (14 bytes)         AFTER DECODE (14 bytes)

;; +--------+----------------+      +--------+----------------+
;; | Length | Actual Content |----->| Length | Actual Content |
;; | 0x000C | "HELLO, WORLD" |      | 0x000C | "HELLO, WORLD" |
;; +--------+----------------+      +--------+----------------+
(deftest No-offset-Lenght-width-two-No-adjustment-no-strip
  (testing "2 bytes length field at offset 0, do not strip header"
    (ok (equalp (let ((enc-config (cl-frame:make-encoder-config :byte-order cl-frame:big-endian
                                                                :length-field-length 2
                                                                :length-adjustment 0
                                                                :length-includes-length-field-length nil))
                      (dec-config (cl-frame:make-decoder-config :byte-order cl-frame:big-endian
                                                                :length-field-offset 0
                                                                :length-field-length 2
                                                                :length-adjustment 0
                                                                :initial-bytes-to-strip 0))
                      (msg (flexi-streams:string-to-octets "HELLO, WORLD")))
                  (bt:make-thread (lambda ()
                                    (let ((conn (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
                                      (unwind-protect (progn
                                                        (cl-frame:write-frame (make-instance 'cl-frame:length-field-based-frame-codec
                                                                                             :encoder-config enc-config
                                                                                             :decoder-config dec-config
                                                                                             :iostream (usocket:socket-stream conn))
                                                                              msg))
                                        (usocket:socket-close conn)))))
                  (usocket:with-client-socket (socket stream "localhost" 1990 :element-type '(unsigned-byte 8))
                    (unwind-protect
                         (cl-frame:read-frame (make-instance 'cl-frame:length-field-based-frame-codec :encoder-config enc-config
                                                                                                      :decoder-config dec-config
                                                                                                      :iostream stream))
                      (usocket:socket-close socket)
                      )))

                (let ((head (make-array 2 :element-type '(unsigned-byte 8)))
                      (body (flexi-streams:string-to-octets "HELLO, WORLD")))
                  (cl-frame:put-uint16 cl-frame:big-endian head 12)

                  (concatenate 'vector head body)))
        "2 bytes length field at offset 0, do not strip header")))

(deftest No-offset-Lenght-width-two-No-adjustment-no-strip-large
  (testing "2 bytes length field at offset 0, do not strip header-large"
    (ok (equalp (let ((enc-config (cl-frame:make-encoder-config :byte-order cl-frame:big-endian
                                                                :length-field-length 2
                                                                :length-adjustment 0
                                                                :length-includes-length-field-length nil))
                      (dec-config (cl-frame:make-decoder-config :byte-order cl-frame:big-endian
                                                                :length-field-offset 0
                                                                :length-field-length 2
                                                                :length-adjustment 0
                                                                :initial-bytes-to-strip 0))
                      (msg (make-array (1- (expt 2 16)) :element-type '(unsigned-byte 8) :initial-element 12)))
                  (bt:make-thread (lambda ()
                                    (let ((conn (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
                                      (unwind-protect (progn
                                                        (cl-frame:write-frame (make-instance 'cl-frame:length-field-based-frame-codec
                                                                                             :encoder-config enc-config
                                                                                             :decoder-config dec-config
                                                                                             :iostream (usocket:socket-stream conn))
                                                                              msg))
                                        (usocket:socket-close conn)))))
                  (usocket:with-client-socket (socket stream "localhost" 1990 :element-type '(unsigned-byte 8))
                    (unwind-protect
                         (cl-frame:read-frame (make-instance 'cl-frame:length-field-based-frame-codec :encoder-config enc-config
                                                                                                      :decoder-config dec-config
                                                                                                      :iostream stream))
                      (usocket:socket-close socket)
                      )))

                (let ((head (make-array 2 :element-type '(unsigned-byte 8)))
                      (body (make-array (1- (expt 2 16)) :element-type '(unsigned-byte 8) :initial-element 12)))
                  (cl-frame:put-uint16 cl-frame:big-endian head (1- (expt 2 16)))

                  (concatenate 'vector head body)))
        "2 bytes length field at offset 0, do not strip header-large")))

;; * 2 bytes length field at offset 0, strip header
;; lengthFieldOffset   = 0
;; lengthFieldLength   = 2
;; lengthAdjustment    = 0
;; initialBytesToStrip = 2 (= the length of the Length field)
;; BEFORE DECODE (14 bytes)         AFTER DECODE (12 bytes)

;; +--------+----------------+      +----------------+
;; | Length | Actual Content |----->| Actual Content |
;; | 0x000C | "HELLO, WORLD" |      | "HELLO, WORLD" |
;; +--------+----------------+      +----------------+

(deftest two-bytes-length-field-at-offset-zero-strip-header
  (testing "2 bytes length field at offset 0, strip header"
    (ok (equalp (let ((enc-config (cl-frame:make-encoder-config :byte-order cl-frame:big-endian
                                                                :length-field-length 2
                                                                :length-adjustment 0
                                                                :length-includes-length-field-length nil))
                      (dec-config (cl-frame:make-decoder-config :byte-order cl-frame:big-endian
                                                                :length-field-offset 0
                                                                :length-field-length 2
                                                                :length-adjustment 0
                                                                :initial-bytes-to-strip 2))
                      (msg (flexi-streams:string-to-octets "HELLO, WORLD")))
                  (bt:make-thread (lambda ()
                                    (let ((conn (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
                                      (unwind-protect (progn
                                                        (cl-frame:write-frame (make-instance 'cl-frame:length-field-based-frame-codec
                                                                                             :encoder-config enc-config
                                                                                             :decoder-config dec-config
                                                                                             :iostream (usocket:socket-stream conn))
                                                                              msg))
                                        (usocket:socket-close conn)))))
                  (usocket:with-client-socket (socket stream "localhost" 1990 :element-type '(unsigned-byte 8))
                    (unwind-protect
                         (cl-frame:read-frame (make-instance 'cl-frame:length-field-based-frame-codec :encoder-config enc-config
                                                                                                      :decoder-config dec-config
                                                                                                      :iostream stream))
                      (usocket:socket-close socket))))
                (flexi-streams:string-to-octets "HELLO, WORLD"))
        "2 bytes length field at offset 0, strip header")))


;; * 2 bytes length field at offset 0, do not strip header,the length field represents the length of the whole message
;; lengthFieldOffset   =  0
;; lengthFieldLength   =  2
;; lengthAdjustment    = -2 (= the length of the Length field)
;; initialBytesToStrip =  0

;; BEFORE DECODE (14 bytes)         AFTER DECODE (14 bytes)
;; +--------+----------------+      +--------+----------------+
;; | Length | Actual Content |----->| Length | Actual Content |
;; | 0x000E | "HELLO, WORLD" |      | 0x000E | "HELLO, WORLD" |
;; +--------+----------------+      +--------+----------------+

(deftest two-bytes-length-field-at-offset-zero-do-not-strip-header-length-includes-length-field-length
  (testing "2 bytes length field at offset 0, do not strip header,the length field represents the length of the whole message"
    (ok (equalp (let ((enc-config (cl-frame:make-encoder-config :byte-order cl-frame:big-endian
                                                                :length-field-length 2
                                                                :length-adjustment 0
                                                                :length-includes-length-field-length t))
                      (dec-config (cl-frame:make-decoder-config :byte-order cl-frame:big-endian
                                                                :length-field-offset 0
                                                                :length-field-length 2
                                                                :length-adjustment -2
                                                                :initial-bytes-to-strip 0))
                      (msg (flexi-streams:string-to-octets "HELLO, WORLD")))
                  (bt:make-thread (lambda ()
                                    (let ((conn (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
                                      (unwind-protect (progn
                                                        (cl-frame:write-frame (make-instance 'cl-frame:length-field-based-frame-codec
                                                                                             :encoder-config enc-config
                                                                                             :decoder-config dec-config
                                                                                             :iostream (usocket:socket-stream conn))
                                                                              msg))
                                        (usocket:socket-close conn)))))
                  (usocket:with-client-socket (socket stream "localhost" 1990 :element-type '(unsigned-byte 8))
                    (unwind-protect
                         (cl-frame:read-frame (make-instance 'cl-frame:length-field-based-frame-codec :encoder-config enc-config
                                                                                                      :decoder-config dec-config
                                                                                                      :iostream stream))
                      (usocket:socket-close socket))))

                (let ((head (make-array 2 :element-type '(unsigned-byte 8)))
                      (body (flexi-streams:string-to-octets "HELLO, WORLD"))
                      (result))
                  (cl-frame:put-uint16 cl-frame:big-endian head 14)

                  (setf result (concatenate 'vector head body))
                  result))
        "2 bytes length field at offset 0, do not strip header,the length field represents the length of the whole message")))

;; * 3 bytes length field at the end of 5 bytes header, do not strip header
;; lengthFieldOffset   = 2 (= the length of Header 1)
;; lengthFieldLength   = 3
;; lengthAdjustment    = 0
;; initialBytesToStrip = 0

;; BEFORE DECODE (17 bytes)                      AFTER DECODE (17 bytes)
;; +----------+----------+----------------+      +----------+----------+----------------+
;; | Header 1 |  Length  | Actual Content |----->| Header 1 |  Length  | Actual Content |
;; |  0xCAFE  | 0x00000C | "HELLO, WORLD" |      |  0xCAFE  | 0x00000C | "HELLO, WORLD" |
;; +----------+----------+----------------+      +----------+----------+----------------+

(deftest three-bytes-length-field-at-the-end-of-five-bytes-header-do-not-strip-header
  (testing "3 bytes length field at the end of 5 bytes header, do not strip header"
    (ok (equalp (let ((enc-config (cl-frame:make-encoder-config :byte-order cl-frame:big-endian
                                                                :length-field-offset 2
                                                                :length-field-length 3
                                                                :length-adjustment 0
                                                                :length-includes-length-field-length nil))
                      (dec-config (cl-frame:make-decoder-config :byte-order cl-frame:big-endian
                                                                :length-field-offset 2
                                                                :length-field-length 3
                                                                :length-adjustment 0
                                                                :initial-bytes-to-strip 0))
                      (msg (flexi-streams:string-to-octets "HELLO, WORLD")))
                  (bt:make-thread (lambda ()
                                    (let ((conn (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
                                      (unwind-protect (progn
                                                        (cl-frame:write-frame (make-instance 'cl-frame:length-field-based-frame-codec
                                                                                             :encoder-config enc-config
                                                                                             :decoder-config dec-config
                                                                                             :iostream (usocket:socket-stream conn))
                                                                              msg :header (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0)))
                                        (usocket:socket-close conn)))))
                  (usocket:with-client-socket (socket stream "localhost" 1990 :element-type '(unsigned-byte 8))
                    (unwind-protect
                         (cl-frame:read-frame (make-instance 'cl-frame:length-field-based-frame-codec :encoder-config enc-config
                                                                                                      :decoder-config dec-config
                                                                                                      :iostream stream))
                      (usocket:socket-close socket))))

                (let ((head (make-array 3 :element-type '(unsigned-byte 8)))
                      (body (flexi-streams:string-to-octets "HELLO, WORLD"))
                      (result))
                  (cl-frame:put-uint24 cl-frame:big-endian head 12)

                  (setf result (concatenate 'vector (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0) head body))
                  result))
        "3 bytes length field at the end of 5 bytes header, do not strip header")))


;; * 3 bytes length field at the beginning of 5 bytes header, do not strip header
;; lengthFieldOffset   = 0
;; lengthFieldLength   = 3
;; lengthAdjustment    = 2 (= the length of Header 1)
;; initialBytesToStrip = 0

;; BEFORE DECODE (17 bytes)                      AFTER DECODE (17 bytes)
;; +----------+----------+----------------+      +----------+----------+----------------+
;; |  Length  | Header 1 | Actual Content |----->|  Length  | Header 1 | Actual Content |
;; | 0x00000C |  0xCAFE  | "HELLO, WORLD" |      | 0x00000C |  0xCAFE  | "HELLO, WORLD" |
;; +----------+----------+----------------+      +----------+----------+----------------+

(deftest three-bytes-length-field-at-the-beiginning-of-5-bytes-header-do-not-strip-header
  (testing "3 bytes length field at the beginning of 5 bytes header, do not strip header"
    (ok (equalp (let ((enc-config (cl-frame:make-encoder-config :byte-order cl-frame:big-endian
                                                                :length-field-offset 0
                                                                :length-field-length 3
                                                                :length-adjustment -2
                                                                :length-includes-length-field-length nil))
                      (dec-config (cl-frame:make-decoder-config :byte-order cl-frame:big-endian
                                                                :length-field-offset 0
                                                                :length-field-length 3
                                                                :length-adjustment 2
                                                                :initial-bytes-to-strip 0))
                      (msg (flexi-streams:string-to-octets "HELLO, WORLD")))
                  (bt:make-thread (lambda ()
                                    (let ((conn (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
                                      (unwind-protect (progn
                                                        (cl-frame:write-frame (make-instance 'cl-frame:length-field-based-frame-codec
                                                                                             :encoder-config enc-config
                                                                                             :decoder-config dec-config
                                                                                             :iostream (usocket:socket-stream conn))
                                                                              (concatenate 'vector (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0) msg)))
                                        (usocket:socket-close conn)))))
                  (usocket:with-client-socket (socket stream "localhost" 1990 :element-type '(unsigned-byte 8))
                    (unwind-protect
                         (cl-frame:read-frame (make-instance 'cl-frame:length-field-based-frame-codec :encoder-config enc-config
                                                                                                      :decoder-config dec-config
                                                                                                      :iostream stream))
                      (usocket:socket-close socket))))

                (let ((head (make-array 3 :element-type '(unsigned-byte 8)))
                      (body (flexi-streams:string-to-octets "HELLO, WORLD"))
                      (result))
                  (cl-frame:put-uint24 cl-frame:big-endian head 12)

                  (setf result (concatenate 'vector head (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0) body))
                  result))
        "3 bytes length field at the beginning of 5 bytes header, do not strip header")))


;; * 2 bytes length field at offset 1 in the middle of 4 bytes header, strip the first header field and the length field
;; lengthFieldOffset   = 1 (= the length of HDR1)
;; lengthFieldLength   = 2
;; lengthAdjustment    = 1 (= the length of HDR2)
;; initialBytesToStrip = 3 (= the length of HDR1 + LEN)

;; BEFORE DECODE (16 bytes)                       AFTER DECODE (13 bytes)
;; +------+--------+------+----------------+      +------+----------------+
;; | HDR1 | Length | HDR2 | Actual Content |----->| HDR2 | Actual Content |
;; | 0xCA | 0x000C | 0xFE | "HELLO, WORLD" |      | 0xFE | "HELLO, WORLD" |
;; +------+--------+------+----------------+      +------+----------------+

(deftest two-bytes-length-field-at-offset-one-in-the-middle-of-four-bytes-header-strip-the-first-header-field-and-the-length-field
  (testing "bytes length field at offset 1 in the middle of 4 bytes header, strip the first header field and the length field"
    (ok (equalp (let ((enc-config (cl-frame:make-encoder-config :byte-order cl-frame:big-endian
                                                                :length-field-offset 1
                                                                :length-field-length 2
                                                                :length-adjustment -1
                                                                :length-includes-length-field-length nil))
                      (dec-config (cl-frame:make-decoder-config :byte-order cl-frame:big-endian
                                                                :length-field-offset 1
                                                                :length-field-length 2
                                                                :length-adjustment 1
                                                                :initial-bytes-to-strip 3))
                      (msg (flexi-streams:string-to-octets "HELLO, WORLD")))
                  (bt:make-thread (lambda ()
                                    (let ((conn (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
                                      (unwind-protect (progn
                                                        (cl-frame:write-frame (make-instance 'cl-frame:length-field-based-frame-codec
                                                                                             :encoder-config enc-config
                                                                                             :decoder-config dec-config
                                                                                             :iostream (usocket:socket-stream conn))
                                                                              (concatenate 'vector (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0) msg ):header (make-array 1 :element-type '(unsigned-byte 8))))
                                        (usocket:socket-close conn)))))
                  (usocket:with-client-socket (socket stream "localhost" 1990 :element-type '(unsigned-byte 8))
                    (unwind-protect
                         (cl-frame:read-frame (make-instance 'cl-frame:length-field-based-frame-codec :encoder-config enc-config
                                                                                                      :decoder-config dec-config
                                                                                                      :iostream stream))
                      (usocket:socket-close socket))))

                (let ((head (make-array 1 :element-type '(unsigned-byte 8)))
                      (body (flexi-streams:string-to-octets "HELLO, WORLD"))
                      (result))
                  ;; (cl-frame:put-uint24 cl-frame:big-endian head 12)
                  (setf (aref head 0) (aref (bit-smasher:octets<- 0) 0))

                  (setf result (concatenate 'vector head body))
                  result))
        "2 bytes length field at offset 1 in the middle of 4 bytes header, strip the first header field and the length field")))

;; * 2 bytes length field at offset 1 in the middle of 4 bytes header, strip the first header field and the length field, the length field represents the length of the whole message
;; lengthFieldOffset   =  1
;; lengthFieldLength   =  2
;; lengthAdjustment    = -3 (= the length of HDR1 + LEN, negative)
;; initialBytesToStrip =  3

;; BEFORE DECODE (16 bytes)                       AFTER DECODE (13 bytes)
;; +------+--------+------+----------------+      +------+----------------+
;; | HDR1 | Length | HDR2 | Actual Content |----->| HDR2 | Actual Content |
;; | 0xCA | 0x0010 | 0xFE | "HELLO, WORLD" |      | 0xFE | "HELLO, WORLD" |
;; +------+--------+------+----------------+      +------+----------------+

(deftest two-bytes-length-field-at-offset-one-in-the-middle-of-four-bytes-header-strip-the-first-header-field-and-the-length-field-the-length-field-represents-the-length-of-the-whole-message
  (testing "2 bytes length field at offset 1 in the middle of 4 bytes header, strip the first header field and the length field, the length field represents the length of the whole message"
    (ok (equalp (let ((enc-config (cl-frame:make-encoder-config :byte-order cl-frame:big-endian
                                                                :length-field-offset 1
                                                                :length-field-length 2
                                                                :length-adjustment 1
                                                                :length-includes-length-field-length t))
                      (dec-config (cl-frame:make-decoder-config :byte-order cl-frame:big-endian
                                                                :length-field-offset 1
                                                                :length-field-length 2
                                                                :length-adjustment -3
                                                                :initial-bytes-to-strip 3))
                      (msg (flexi-streams:string-to-octets "HELLO, WORLD")))
                  (bt:make-thread (lambda ()
                                    (let ((conn (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
                                      (unwind-protect (progn
                                                        (cl-frame:write-frame (make-instance 'cl-frame:length-field-based-frame-codec
                                                                                             :encoder-config enc-config
                                                                                             :decoder-config dec-config
                                                                                             :iostream (usocket:socket-stream conn))
                                                                              (concatenate 'vector (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0) msg ):header (make-array 1 :element-type '(unsigned-byte 8))))
                                        (usocket:socket-close conn)))))
                  (usocket:with-client-socket (socket stream "localhost" 1990 :element-type '(unsigned-byte 8))
                    (unwind-protect
                         (cl-frame:read-frame (make-instance 'cl-frame:length-field-based-frame-codec :encoder-config enc-config
                                                                                                      :decoder-config dec-config
                                                                                                      :iostream stream))
                      (usocket:socket-close socket))))

                (let ((head (make-array 1 :element-type '(unsigned-byte 8)))
                      (body (flexi-streams:string-to-octets "HELLO, WORLD"))
                      (result))
                  ;; (cl-frame:put-uint24 cl-frame:big-endian head 12)
                  (setf (aref head 0) (aref (bit-smasher:octets<- 0) 0))

                  (setf result (concatenate 'vector head body))
                  result))
        "2 bytes length field at offset 1 in the middle of 4 bytes header, strip the first header field and the length field, the length field represents the length of the whole message")))
