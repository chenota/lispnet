;;;; lisp-network.lisp

(in-package #:lisp-network)

(defun gethash-or (key table default)
  (multiple-value-bind
      (val exists)
      (gethash key table)
    (if
     exists
     val
     (setf (gethash key table) default))))

(defclass digraph ()
    ((nodes :initform (make-hash-table :test 'equal))
     (succ :initform (make-hash-table :test 'equal))
     (pred :initform (make-hash-table :test 'equal))))

(defun make-digraph () (make-instance 'digraph))

(defmethod set-node ((d digraph) node &rest args)
  (unless (evenp (length args)) (error "expected even number of arguments"))
  (let
      ((table (gethash-or node (slot-value d 'nodes) (make-hash-table :test 'equal))))
    (loop for (k v) on args by #'cddr do
            (if
             (keywordp k)
             (setf (gethash k table) v)
             (error "property keys must be keywords")))))

(defmethod nodep ((d digraph) node)
    (multiple-value-bind
        (node exists)
        (gethash node (slot-value d 'nodes))
        (declare (ignore node))
        exists))

(defmethod node-property ((d digraph) node key)
  (unless (keywordp key) (error "property keys must be keywords"))
  (multiple-value-bind
      (node node-exists)
      (gethash node (slot-value d 'nodes))
    (if
     node-exists
     (multiple-value-bind
         (value value-exists)
         (gethash key node)
       (values value t value-exists))
     (values nil nil nil))))

(defsetf node-property (d node key) (new-value)
    `(progn (set-node ,d ,node ,key ,new-value) ,new-value))
