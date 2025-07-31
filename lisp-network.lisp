;;;; lisp-network.lisp

(in-package #:lisp-network)

(defclass digraph ()
    ((nodes :initform (make-hash-table :test 'equal) :reader nodes)
     (pred :initform (make-hash-table :test 'equal) :reader pred)
     (succ :initform (make-hash-table :test 'equal) :reader succ)))

(defun make-digraph () (make-instance 'digraph))