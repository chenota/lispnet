;;;; digraph.lisp

(in-package #:lispnet)

(defclass digraph ()
    ((nodes :initform (make-hash-table :test 'equal))
     (succ :initform (make-hash-table :test 'equal))
     (pred :initform (make-hash-table :test 'equal)))
  (:documentation "A directed graph."))

(defmethod print-object ((d digraph) stream)
  (print-unreadable-object (d stream :type t :identity t)
    (format stream "V=~A E=~A"
      (node-count d)
      (edge-count d))))

(defmethod set-node ((d digraph) node &rest args)
  "Add a node or update an existing node's properties."
  (unless (evenp (length args)) (error 'uneven-argument-error))
  (let
      ((table (gethash-or node (slot-value d 'nodes) (make-hash-table :test 'equal))))
    ;; Add properties.
    (loop for (k v) on args by #'cddr
          do (check-type k keyword "a keyword key")
          do (setf (gethash k table) v))))

(defmethod node-property ((d digraph) node key)
  "Retreive a node property"
  (check-type key keyword "a keyword key")
  (handler-case
      (gethash-multi (slot-value d 'nodes) node key)
    (nested-hash-not-found-error
     (e)
     (if
      (= (failure-level e) 0)
      (error 'node-not-found-error :node node)
      (error 'property-not-found-error :prop key)))))

(defsetf node-property (d node key) (new-value)
  `(progn (set-node ,d ,node ,key ,new-value) ,new-value))

(defmethod set-edge ((d digraph) begin end &rest args)
  "Add an edge or update an existing edge's properties."
  (unless (evenp (length args)) (error 'uneven-argument-error))
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
  (handler-case
      (gethash-multi (slot-value d 'succ) begin end key)
    (nested-hash-not-found-error
     (e)
     (if (< (failure-level e) 2)
         (error 'edge-not-found-error :begin begin :end end)
         (error 'property-not-found-error :prop key)))))

(defsetf edge-property (d begin end key) (new-value)
  `(progn (set-edge ,d ,begin ,end ,key ,new-value) ,new-value))

(defmethod in-degree ((d digraph) node)
  "Calculate the in-degree of a node."
  (unless (node-p d node) (error 'node-not-found-error :node node))
  (multiple-value-bind
      (pred-table exists)
      (gethash node (slot-value d 'pred))
    (if
     exists
     (hash-table-count pred-table)
     0)))

(defmethod out-degree ((d digraph) node)
  "Calculate the out-degree of a node."
  (unless (node-p d node) (error 'node-not-found-error :node node))
  (multiple-value-bind
      (succ-table exists)
      (gethash node (slot-value d 'succ))
    (if
     exists
     (hash-table-count succ-table)
     0)))

(defmethod in-strength ((d digraph) node weight &key (adder #'+) (initial-value nil init-p))
  "Calculate the in-strength of a node relative to a property."
  (check-type weight (or keyword function) "a keyword or function")
  (unless (node-p d node) (error 'node-not-found-error :node node))
  (unless (predecessor d node) (return-from in-strength initial-value))
  (labels
      ((getweight
        (start end)
        (if (functionp weight)
            (funcall weight d start end)
            (edge-property d start end weight))))
    (let* ((pred-list (predecessor d node))
           (init (if init-p initial-value (getweight (car pred-list) node)))
           (seq (if init-p pred-list (cdr pred-list))))
      (reduce (lambda (acc start-node) (funcall adder (getweight start-node node) acc)) seq :initial-value init))))

(defmethod out-strength ((d digraph) node weight &key (adder #'+) (initial-value nil init-p))
  "Calculate the out-strength of a node relative to a property."
  (check-type weight (or keyword function) "a keyword or function")
  (unless (node-p d node) (error 'node-not-found-error :node node))
  (unless (successor d node) (return-from out-strength initial-value))
  (labels
      ((getweight
        (start end)
        (if (functionp weight)
            (funcall weight d start end)
            (edge-property d start end weight))))
    (let* ((succ-list (successor d node))
           (init (if init-p initial-value (getweight node (car succ-list))))
           (seq (if init-p succ-list (cdr succ-list))))
      (reduce (lambda (acc end-node) (funcall adder (getweight node end-node) acc)) seq :initial-value init))))

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
  (unless (node-p d node) (error 'node-not-found-error :node node))
  (multiple-value-bind
      (succ-table exists)
      (gethash node (slot-value d 'succ))
    (if
     exists
     (loop for key being the hash-keys of succ-table collect key)
     nil)))

(defmethod predecessor ((d digraph) node)
  "Retrieve a list containing all predecessors of a node."
  (unless (node-p d node) (error 'node-not-found-error :node node))
  (multiple-value-bind
      (pred-table exists)
      (gethash node (slot-value d 'pred))
    (if
     exists
     (loop for key being the hash-keys of pred-table collect key)
     nil)))

(defmethod unset-node ((d digraph) node)
  "Remove a node and all associated edges from the graph."
  (unless (node-p d node) (error 'node-not-found-error :node node))
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
            (remhash node (gethash key succ-table)))))

(defmethod unset-edge ((d digraph) begin end)
  "Remove an edge from the graph."
  (unless (edge-p d begin end) (error 'edge-not-found-error :begin begin :end end))
  (progn
   (multiple-value-bind
       (table exists)
       (gethash begin (slot-value d 'succ))
     (when exists (remhash end table)))
   (multiple-value-bind
       (table exists)
       (gethash end (slot-value d 'pred))
     (when exists (remhash begin table)))))

(defmethod node-count ((d digraph))
  "Retrieve the number of nodes in the graph."
  (hash-table-count (slot-value d 'nodes)))

(defmethod edge-count ((d digraph))
  "Retrieve the number of edges in the graph."
  (let ((succ (slot-value d 'succ)))
    (loop for begin being the hash-keys of succ sum
            (hash-table-count (gethash begin succ)))))