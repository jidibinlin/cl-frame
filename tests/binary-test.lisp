;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-frame/tests.binary
  (:use #:cl
        #:rove))

(in-package #:cl-frame/tests.binary)

(deftest binary-bigendian
  (testing "put uint16"
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
        "put uint16"))

  (testing "put uint24"
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
        "put uint24"))

  (testing "put uint32"
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
        "put uint32"))

  (testing "put uint64"
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
        "put uint64"))

  )

(deftest binary-littlendian
  (testing "put uint16"
    (ok (and (= 1
                (let ((endian cl-frame:little-endian)
                      (buf (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint16 endian buf 1)
                  (bit-smasher:int<- (reverse buf))
                  )
                )
             (= 255
                (let ((endian cl-frame:little-endian)
                      (buf (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint16 endian buf 255)
                  (bit-smasher:int<- (reverse buf))
                  )
                )
             (= 65535
                (let ((endian cl-frame:little-endian)
                      (buf (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint16 endian buf 65535)
                  (bit-smasher:int<- (reverse buf))
                  )
                )
             )
        "put uint16"))

  (testing "put uint24"
    (ok (and (= 1
                (let ((endian cl-frame:little-endian)
                      (buf (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint24 endian buf 1)
                  (bit-smasher:int<- (reverse buf))
                  )
                )
             (= 255
                (let ((endian cl-frame:little-endian)
                      (buf (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint24 endian buf 255)
                  (bit-smasher:int<- (reverse buf))
                  )
                )
             (= (1- (expt 2 24))
                (let ((endian cl-frame:little-endian)
                      (buf (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint24 endian buf (1- (expt 2 24)))
                  (bit-smasher:int<- (reverse buf))
                  )
                )
             )
        "put uint24"))

  (testing "put uint32"
    (ok (and (= 1
                (let ((endian cl-frame:little-endian)
                      (buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint32 endian buf 1)
                  (bit-smasher:int<- (reverse buf))
                  )
                )
             (= 255
                (let ((endian cl-frame:little-endian)
                      (buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint32 endian buf 255)
                  (bit-smasher:int<- (reverse buf))
                  )
                )
             (= (1- (expt 2 32))
                (let ((endian cl-frame:little-endian)
                      (buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint32 endian buf (1- (expt 2 32)))
                  (bit-smasher:int<- (reverse buf))
                  )
                )
             )
        "put uint32"))

  (testing "put uint64"
    (ok (and (= 1
                (let ((endian cl-frame:little-endian)
                      (buf (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint64 endian buf 1)
                  (bit-smasher:int<- (reverse buf))
                  )
                )
             (= 255
                (let ((endian cl-frame:little-endian)
                      (buf (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint64 endian buf 255)
                  (bit-smasher:int<- (reverse buf))
                  )
                )
             (= (1- (expt 2 64))
                (let ((endian cl-frame:little-endian)
                      (buf (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (cl-frame:put-uint64 endian buf (1- (expt 2 64)))
                  (bit-smasher:int<- (reverse buf))
                  )
                )
             )
        "put uint64"))
  )
