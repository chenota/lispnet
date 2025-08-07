;;;; predicate.lisp

(in-package #:lispnet)

(defmethod node-p ((d digraph) node)
  (multiple-value-bind
      (node exists)
      (gethash node (slot-value d 'nodes))
    (declare (ignore node))
    exists))

(defmethod edge-p ((d digraph) begin end)
  (multiple-value-bind
      (value exists-path)
      (gethash-multi (slot-value d 'succ) begin end)
    (declare (ignore value))
    (first exists-path)))

(defmethod node-property-p ((d digraph) node key)
  (unless (keywordp key) (error "property keys must be keywords"))
  (multiple-value-bind
      (property exists-path)
      (gethash-multi (slot-value d 'nodes) node key)
    (declare (ignore property))
    (values (first exists-path) (second exists-path))))

(defmethod edge-property-p ((d digraph) begin end key)
  (unless (keywordp key) (error "property keys must be keywords"))
  (multiple-value-bind
      (property exists-path)
      (gethash-multi (slot-value d 'succ) begin end key)
    (declare (ignore property))
    (values (first exists-path) (and (second exists-path) (third exists-path)))))