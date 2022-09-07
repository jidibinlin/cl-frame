;;;; cl-frame.asd

(asdf:defsystem #:cl-frame
  :description "Describe cl-frame here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on(#:bit-smasher)
  :components ((:file "package")
               (:file "binary")
               (:file "cl-frame")))
