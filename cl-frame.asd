;;;; cl-frame.asd

(asdf:defsystem #:cl-frame
  :description "Describe cl-frame here"
  :author "2694273649@qq.com"
  :license  "MIT-style"
  :version "0.0.1"
  :serial t
  :depends-on(#:bit-smasher
              #:flexi-streams)
  :components ((:file "package")
               (:file "binary")
               (:file "cl-frame")))

(asdf:defsystem #:cl-frame/tests
  :name "cl-frame tests"
  :author "2694273649@qq.com"
  :license "MIT-style"
  :version "0.0.1"
  :depends-on (:cl-frame
               :rove
               :usocket
               :bt-semaphore
               :flexi-streams)
  :serial t
  :pathname "tests/"
  :components((:module "cl-frame/tests.binary"
               :serial t
               :pathname ""
               :components ((:file "binary-test")))
              (:module "cl-frame/tests.read-write-frame"
               :serial t
               :pathname ""
               :components ((:file "read-write-frame-tests"))))
  )
