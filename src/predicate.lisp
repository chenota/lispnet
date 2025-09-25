;;;; predicate.lisp

(in-package #:lispnet)

(defmethod node-p ((d digraph) node)
  "Checks if a node is in the graph."
  (multiple-value-bind
      (node exists)
      (gethash node (slot-value d 'nodes))
    (declare (ignore node))
    exists))

(defmethod edge-p ((d digraph) begin end)
  "Checks if an edge is in the graph."
  (handler-case
      (gethash-multi (slot-value d 'succ) begin end)
    (nested-hash-not-found-error (e) (declare (ignore e)) nil)
    (:no-error (result) (declare (ignore result)) t)))

(defmethod node-property-p ((d digraph) node key)
  "Checks if a node has a particular property."
  (check-type key keyword "a keyword key")
  (handler-case
      (gethash-multi (slot-value d 'nodes) node key)
    (nested-hash-not-found-error (e) (declare (ignore e)) nil)
    (:no-error (result) (declare (ignore result)) t)))

(defmethod edge-property-p ((d digraph) begin end key)
  "Checks if an edge has a particular property."
  (check-type key keyword "a keyword key")
  (handler-case
      (gethash-multi (slot-value d 'succ) begin end key)
    (nested-hash-not-found-error (e) (declare (ignore e)) nil)
    (:no-error (result) (declare (ignore result)) t)))