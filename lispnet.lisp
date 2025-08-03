;;;; lispnet.lisp

(in-package #:lispnet)

(defun gethash-or (key table default)
  (multiple-value-bind
      (val exists)
      (gethash key table)
    (if
     exists
     val
     (setf (gethash key table) default))))

(defun gethash-multi (table &rest args)
  (labels
      ((gethash-multi-helper
        (keys current exists-path)
        (cond
         ((not keys)
           (values current (reverse exists-path)))
         ((not (typep current 'hash-table))
           (gethash-multi-helper (cdr keys) nil (cons nil exists-path)))
         (t
           (multiple-value-bind (next exists)
               (gethash (car keys) current)
             (gethash-multi-helper (cdr keys) next (cons exists exists-path)))))))
    (gethash-multi-helper args table '())))

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
      (value exists-path)
      (gethash-multi (slot-value d 'node) node key)
    (values value (first exists-path) (second exists-path))))

(defsetf node-property (d node key) (new-value)
  `(progn (set-node ,d ,node ,key ,new-value) ,new-value))

(defmethod set-edge ((d digraph) begin end &rest args)
  (unless (evenp (length args)) (error "expected an even number of arguments"))
  (set-node d begin)
  (set-node d end)
  (let
      ((pred-table (gethash-or end (slot-value d 'pred) (make-hash-table :test 'equal))))
    (setf (gethash begin pred-table) nil))
  (let*
      ((succ-table (gethash-or begin (slot-value d 'succ) (make-hash-table :test 'equal)))
       (props (gethash-or end succ-table (make-hash-table :test 'equal))))
    (loop for (k v) on args by #'cddr do
            (if
             (keywordp k)
             (setf (gethash k props) v)
             (error "property keys must be keywords")))))

(defmethod edgep ((d digraph) begin end)
  (multiple-value-bind
      (value exists-path)
      (gethash-multi (slot-value d 'succ) begin end)
    (declare (ignore value))
    (every #'identity exists-path)))

(defmethod edge-property ((d digraph) begin end key)
  (unless (keywordp key) (error "property keys must be keywords"))
  (multiple-value-bind
      (value exists-path)
      (gethash-multi (slot-value d 'succ) begin end key)
    (values value (and (first exists-path) (second exists-path)) (third exists-path))))

(defsetf edge-property (d begin end key) (new-value)
  `(progn (set-edge ,d ,begin ,end ,key ,new-value) ,new-value))

(defmethod in-degree ((d digraph) node)
  (if
   (nodep d node)
   (multiple-value-bind
       (pred-table exists)
       (gethash node (slot-value d 'pred))
     (if
      exists
      (values (hash-table-count pred-table) t)
      (values 0 t)))
   (values 0 nil)))

(defmethod out-degree ((d digraph) node)
  (if
   (nodep d node)
   (multiple-value-bind
       (succ-table exists)
       (gethash node (slot-value d 'succ))
     (if
      exists
      (values (hash-table-count succ-table) t)
      (values 0 t)))
   (values 0 nil)))