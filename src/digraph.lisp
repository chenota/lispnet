;;;; digraph.lisp

(in-package #:lispnet)

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

(defmethod node-property ((d digraph) node key)
  (unless (keywordp key) (error "property keys must be keywords"))
  (multiple-value-bind
      (value exists-path)
      (gethash-multi (slot-value d 'node) node key)
    (values value (second exists-path) (first exists-path))))

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

(defmethod edge-property ((d digraph) begin end key)
  (check-type key keyword "a keyword key")
  (multiple-value-bind
      (value exists-path)
      (gethash-multi (slot-value d 'succ) begin end key)
    (values value (and (third exists-path) (second exists-path)) (first exists-path))))

(defsetf edge-property (d begin end key) (new-value)
  `(progn (set-edge ,d ,begin ,end ,key ,new-value) ,new-value))

(defmethod in-degree ((d digraph) node)
  (if
   (node-p d node)
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
   (node-p d node)
   (multiple-value-bind
       (succ-table exists)
       (gethash node (slot-value d 'succ))
     (if
      exists
      (values (hash-table-count succ-table) t)
      (values 0 t)))
   (values 0 nil)))

(defmethod in-strength ((d digraph) node key &key (operation #'+) (init nil init-p))
  (check-type key keyword "a keyword key")
  (if
   (node-p d node)
   (multiple-value-bind
       (pred-table exists)
       (gethash node (slot-value d 'pred))
     (if
      exists
      (loop for start-node being the hash-keys of pred-table
            with result = init
            do (multiple-value-bind
                   (prop edge-exists prop-exists)
                   (edge-property d start-node node key)
                 (declare (ignore edge-exists))
                 (when
                  prop-exists
                  (if
                   init-p
                   (setf result (funcall operation prop result))
                   (progn
                    (setf result prop)
                    (setf init-p t)))))
            finally (return (values result t)))
      (values init t)))
   (values nil nil)))

(defmethod out-strength ((d digraph) node key &key (operation #'+) (init nil init-p))
  (unless (keywordp key) (error "property keys must be keywords"))
  (if
   (node-p d node)
   (multiple-value-bind
       (succ-table exists)
       (gethash node (slot-value d 'succ))
     (if
      exists
      (loop for end-node being the hash-keys of succ-table
            with result = init
            do (multiple-value-bind
                   (prop edge-exists prop-exists)
                   (edge-property d node end-node key)
                 (declare (ignore edge-exists))
                 (when
                  prop-exists
                  (if
                   init-p
                   (setf result (funcall operation prop result))
                   (progn
                    (setf result prop)
                    (setf init-p t)))))
            finally (return (values result t)))
      (values init t)))
   (values nil nil)))

(defmethod nodes ((d digraph))
  (loop for key being the hash-keys of (slot-value d 'nodes) collect key))

(defmethod successor ((d digraph) node)
  (if
   (node-p d node)
   (multiple-value-bind
       (succ-table exists)
       (gethash node (slot-value d 'succ))
     (if
      exists
      (values
        (loop for key being the hash-keys of succ-table collect key)
        t)
      (values nil t)))
   (values nil nil)))

(defmethod predecessor ((d digraph) node)
  (if
   (node-p d node)
   (multiple-value-bind
       (pred-table exists)
       (gethash node (slot-value d 'pred))
     (if
      exists
      (values
        (loop for key being the hash-keys of pred-table collect key)
        t)
      (values nil t)))
   (values nil nil)))

(defmethod unset-node ((d digraph) node)
  (if
   (node-p d node)
   (let ((pred-table (slot-value d 'pred))
         (succ-table (slot-value d 'succ)))
     (remhash node (slot-value d 'nodes))
     (remhash node (slot-value d 'succ))
     (remhash node (slot-value d 'pred))
     (loop for key being the hash-keys of pred-table do
             (remhash node (gethash key pred-table)))
     (loop for key being the hash-keys of succ-table do
             (remhash node (gethash key succ-table)))
     t)
   nil))

(defmethod unset-edge ((d digraph) begin end)
  (if
   (edge-p d begin end)
   (progn
    (multiple-value-bind
        (table exists)
        (gethash begin (slot-value d 'succ))
      (when exists (remhash end table)))
    (multiple-value-bind
        (table exists)
        (gethash end (slot-value d 'pred))
      (when exists (remhash begin table)))
    t)
   nil))

(defmethod node-count ((d digraph))
  (hash-table-count (slot-value d 'nodes)))

(defmethod edge-count ((d digraph))
  (let ((succ (slot-value d 'succ)))
    (loop for begin being the hash-keys of succ sum
            (hash-table-count (gethash begin succ)))))