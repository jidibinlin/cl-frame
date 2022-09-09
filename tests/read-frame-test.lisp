;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-frame/test.read-frame
  (:use #:cl
        #:rove
        #:usocket)
  (:shadowing-import-from #:rove
                          #:*debug-on-error*))

(in-package #:cl-frame/test.read-frame)

;; (defun test ()
;;   "Run all tests in the test suite."
;;   (rove:run-suite *package*)
;;   )

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
(deftest 2blo0ns
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
                                    (let* ((server (usocket:socket-listen "localhost" 1990 :element-type '(unsigned-byte 8)))
                                           (sock (usocket:socket-accept server :element-type '(unsigned-byte 8))))
                                      (cl-frame:write-frame (make-instance 'cl-frame:length-field-based-frame-codec
                                                                           :encoder-config enc-config
                                                                           :decoder-config dec-config
                                                                           :iostream (usocket:socket-stream sock))
                                                            msg)
                                      (usocket:socket-close sock)
                                      (usocket:socket-close server))))
                  (sleep 1)

                  (let* ((sock (usocket:socket-connect "localhost" 1990))
                         (result))
                    (setf result (cl-frame:read-frame (make-instance 'cl-frame:length-field-based-frame-codec :encoder-config enc-config
                                                                                                              :decoder-config dec-config
                                                                                                              :iostream (usocket:socket-stream sock))))
                    (format t "~a" result)
                    (usocket:socket-close sock)
                    result
                    ))

                (let ((head (make-array 2 :element-type '(unsigned-byte 8)))
                      (body (flexi-streams:string-to-octets "HELLO, WORLD"))
                      (result))
                  (cl-frame:put-uint16 cl-frame:big-endian head 12)

                  (setf result (concatenate 'vector head body))
                  (format t "~a" result)
                  result))
        "2 bytes length field at offset 0, do not strip header")))


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
