;;;; lispnet.asd

(asdf:defsystem #:lispnet
  :description "Network library for Common Lisp"
  :author "Alex Chenot <alex@chenot.dev>"
  :license "MIT"
  :version "1.0.0"
  :serial t
  :components ((:file "package")
               (:module "src"
                        :components ((:file "digraph")
                                     (:file "hash")
                                     (:file "predicate")
                                     (:file "viz")
                                     (:file "queue")
                                     (:file "search")
                                     (:file "error")
                                     (:file "make")))))
