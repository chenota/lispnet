;;;; lispnet.asd

(asdf:defsystem #:lispnet
  :description "Network library for Common Lisp"
  :author "Alex Chenot <alex@chenot.dev>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:module "src"
                        :components ((:file "digraph")
                                     (:file "hash")
                                     (:file "predicate")))))
