;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-frame/tests.binary
  (:use #:cl
        #:rove)
  (:export :run))

(in-package #:cl-frame/tests.binary)

(defun test ()
  "Run all tests in the tests suite."
  (rove:run-suite *package*)
  )

(deftest bigendian-put-uint16
    (testing "bigendian put uint16"
      (ok (and (= 1
                  (let ((endian cl-frame:big-endian)
                        (buf (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0)))
                    (cl-frame:put-uint16 endian buf 1)
                    (bit-smasher:int<- buf)
                    )
                  )
               (= 255
                  (let ((endian cl-frame:big-endian)
                        (buf (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0)))
                    (cl-frame:put-uint16 endian buf 255)
                    (bit-smasher:int<- buf)
                    )
                  )
               (= 65535
                  (let ((endian cl-frame:big-endian)
                        (buf (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0)))
                    (cl-frame:put-uint16 endian buf 65535)
                    (bit-smasher:int<- buf)
                    )
                  )
               )
          "bigendian put uint16")
      ))

(deftest bigendian-put-uint24
    (testing "bigendian put uint24"
      (ok (and (= 1
                  (let ((endian cl-frame:big-endian)
                        (buf (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0)))
                    (cl-frame:put-uint24 endian buf 1)
                    (bit-smasher:int<- buf)
                    )
                  )
               (= 255
                  (let ((endian cl-frame:big-endian)
                        (buf (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0)))
                    (cl-frame:put-uint24 endian buf 255)
                    (bit-smasher:int<- buf)
                    )
                  )
               (= (1- (expt 2 24))
                  (let ((endian cl-frame:big-endian)
                        (buf (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0)))
                    (cl-frame:put-uint24 endian buf (1- (expt 2 24)))
                    (bit-smasher:int<- buf)
                    )
                  )
               )
          "bigendian put uint24")
      ))

(deftest bigendian-put-uint32
    (testing "bigendian put uint32"
      (ok (and (= 1
                  (let ((endian cl-frame:big-endian)
                        (buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
                    (cl-frame:put-uint32 endian buf 1)
                    (bit-smasher:int<- buf)
                    )
                  )
               (= 255
                  (let ((endian cl-frame:big-endian)
                        (buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
                    (cl-frame:put-uint32 endian buf 255)
                    (bit-smasher:int<- buf)
                    )
                  )
               (= (1- (expt 2 32))
                  (let ((endian cl-frame:big-endian)
                        (buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
                    (cl-frame:put-uint32 endian buf (1- (expt 2 32)))
                    (bit-smasher:int<- buf)
                    )
                  )
               )
          "bigendian put uint32")
      ))

(deftest bigendian-put-uint64
  (testing "bigendian put uint64"
    (ok (and (= 1
                (let ((endian cl-frame:big-endian)
                      (buf (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint64 endian buf 1)
                  (bit-smasher:int<- buf)
                  )
                )
             (= 255
                (let ((endian cl-frame:big-endian)
                      (buf (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint64 endian buf 255)
                  (bit-smasher:int<- buf)
                  )
                )
             (= (1- (expt 2 64))
                (let ((endian cl-frame:big-endian)
                      (buf (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint64 endian buf (1- (expt 2 64)))
                  (bit-smasher:int<- buf)
                  )
                )
             )
        "bigendian put uint64")
    ))

(deftest littlendian-put-uint16
  (testing "littlendian put uint16"
    (ok (and (= 1
                (let ((endian cl-frame:big-endian)
                      (buf (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint16 endian buf 1)
                  (bit-smasher:int<- buf)
                  )
                )
             (= 255
                (let ((endian cl-frame:big-endian)
                      (buf (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint16 endian buf 255)
                  (bit-smasher:int<- buf)
                  )
                )
             (= 65535
                (let ((endian cl-frame:big-endian)
                      (buf (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint16 endian buf 65535)
                  (bit-smasher:int<- buf)
                  )
                )
             )
        "littlendian put uint16")
    ))

(deftest littlendian-put-uint24
  (testing "littlendian put uint24"
    (ok (and (= 1
                (let ((endian cl-frame:big-endian)
                      (buf (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint24 endian buf 1)
                  (bit-smasher:int<- buf)
                  )
                )
             (= 255
                (let ((endian cl-frame:big-endian)
                      (buf (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint24 endian buf 255)
                  (bit-smasher:int<- buf)
                  )
                )
             (= (1- (expt 2 24))
                (let ((endian cl-frame:big-endian)
                      (buf (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint24 endian buf (1- (expt 2 24)))
                  (bit-smasher:int<- buf)
                  )
                )
             )
        "littlendian put uint24")
    ))

(deftest littlendian-put-uint32
  (testing "littlendian put uint32"
    (ok (and (= 1
                (let ((endian cl-frame:big-endian)
                      (buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint32 endian buf 1)
                  (bit-smasher:int<- buf)
                  )
                )
             (= 255
                (let ((endian cl-frame:big-endian)
                      (buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint32 endian buf 255)
                  (bit-smasher:int<- buf)
                  )
                )
             (= (1- (expt 2 32))
                (let ((endian cl-frame:big-endian)
                      (buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint32 endian buf (1- (expt 2 32)))
                  (bit-smasher:int<- buf)
                  )
                )
             )
        "littlendian put uint32")
    ))

(deftest littlendian-put-uint64
  (testing "littlendian put uint64"
    (ok (and (= 1
                (let ((endian cl-frame:big-endian)
                      (buf (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint64 endian buf 1)
                  (bit-smasher:int<- buf)
                  )
                )
             (= 255
                (let ((endian cl-frame:big-endian)
                      (buf (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint64 endian buf 255)
                  (bit-smasher:int<- buf)
                  )
                )
             (= (1- (expt 2 64))
                (let ((endian cl-frame:big-endian)
                      (buf (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint64 endian buf (1- (expt 2 64)))
                  (bit-smasher:int<- buf)
                  )
                )
             )
        "littlendian put uint64")
    ))
