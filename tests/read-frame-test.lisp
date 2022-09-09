;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-frame.test.read-frame
  (:use #:cl
        #:rove)
  (:export :run))

(in-package #:cl-frame.test.read-frame)



(defun run ()
  "Run all tests in the test suite."
  (rove:run-suite *package*)
  )
