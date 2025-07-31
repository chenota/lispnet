;;;; lisp-network.asd

(asdf:defsystem #:lisp-network
  :description "Network library for Common Lisp"
  :author "Alex Chenot"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "lisp-network")))
