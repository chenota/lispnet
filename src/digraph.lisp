;;;; digraph.lisp

(in-package #:lispnet)

(defclass digraph ()
    ((nodes :initform (make-hash-table :test 'equal))
     (succ :initform (make-hash-table :test 'equal))
     (pred :initform (make-hash-table :test 'equal)))
  (:documentation "A directed graph."))

(defun make-digraph ()
  "Create an empty directed graph."
  (make-instance 'digraph))

(defmethod set-node ((d digraph) node &rest args)
  "Add a node or update an existing node's properties."
  (unless (evenp (length args)) (error "expected even number of arguments"))
  (let
      ((table (gethash-or node (slot-value d 'nodes) (make-hash-table :test 'equal))))
    ;; Add properties.
    (loop for (k v) on args by #'cddr
          do (check-type k keyword "a keyword key")
          do (setf (gethash k table) v))))

(defmethod node-property ((d digraph) node key)
  "Retreive a node property"
  (check-type key keyword "a keyword key")
  (multiple-value-bind
      (value exists-path)
      (gethash-multi (slot-value d 'node) node key)
    (values value (second exists-path) (first exists-path))))

(defsetf node-property (d node key) (new-value)
  `(progn (set-node ,d ,node ,key ,new-value) ,new-value))

(defmethod set-edge ((d digraph) begin end &rest args)
  "Add an edge or update an existing edge's properties."
  (unless (evenp (length args)) (error "expected an even number of arguments"))
  (set-node d begin)
  (set-node d end)
  ;; Create an entry in the predecessors table.
  (setf
    (gethash
      begin
      (gethash-or end (slot-value d 'pred) (make-hash-table :test 'equal)))
    nil)
  (let*
      ;; Create an entry in the successors table.
    ((succ-table (gethash-or begin (slot-value d 'succ) (make-hash-table :test 'equal)))
     (props (gethash-or end succ-table (make-hash-table :test 'equal))))
    ;; Add properties.
    (loop for (k v) on args by #'cddr
          do (check-type k keyword "a keyword key")
          do (setf (gethash k props) v))))

(defmethod edge-property ((d digraph) begin end key)
  "Retrieve an edge property."
  (check-type key keyword "a keyword key")
  (multiple-value-bind
      (value exists-path)
      (gethash-multi (slot-value d 'succ) begin end key)
    (values value (and (third exists-path) (second exists-path)) (first exists-path))))

(defsetf edge-property (d begin end key) (new-value)
  `(progn (set-edge ,d ,begin ,end ,key ,new-value) ,new-value))

(defmethod in-degree ((d digraph) node)
  "Calculate the in-degree of a node."
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
  "Calculate the out-degree of a node."
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
  "Calculate the in-strength of a node relative to a property. Ignores edges which are missing the specified property."
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
  "Calculate the out-strength of a node relative to a property. Ignores edges which are missing the specified property."
  (check-type key keyword "a keyword key")
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
  "Retrieve a list containing each node in the graph."
  (loop for key being the hash-keys of (slot-value d 'nodes) collect key))

(defmethod edges ((d digraph))
  "Retrieve a list containing each edge in the graph."
  (loop for start being the hash-keys of (slot-value d 'succ) append
          (loop for end being the hash-keys of (gethash start (slot-value d 'succ))
                collect (cons start end))))

(defmethod successor ((d digraph) node)
  "Retrieve a list containing all successors of a node."
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
  "Retrieve a list containing all predecessors of a node."
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
  "Remove a node and all associated edges from the graph."
  (if
   (node-p d node)
   (let ((pred-table (slot-value d 'pred))
         (succ-table (slot-value d 'succ)))
     ;; Remove node from all top-level hash tables.
     (remhash node (slot-value d 'nodes))
     (remhash node (slot-value d 'succ))
     (remhash node (slot-value d 'pred))
     ;; Remove references to node buried in other tables.
     (loop for key being the hash-keys of pred-table do
             (remhash node (gethash key pred-table)))
     (loop for key being the hash-keys of succ-table do
             (remhash node (gethash key succ-table)))
     t)
   nil))

(defmethod unset-edge ((d digraph) begin end)
  "Remove an edge from the graph."
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
  "Retrieve the number of nodes in the graph."
  (hash-table-count (slot-value d 'nodes)))

(defmethod edge-count ((d digraph))
  "Retrieve the number of edges in the graph."
  (let ((succ (slot-value d 'succ)))
    (loop for begin being the hash-keys of succ sum
            (hash-table-count (gethash begin succ)))))