;;;; search.lisp

(in-package #:lispnet)

(defun weight1 (digraph current neighbor)
  "Returns an edge weight of one."
  (declare (ignore digraph) (ignore current) (ignore neighbor))
  1)

(defun heuristic0 (digraph current goal)
  "Returns a heuristic value of zero."
  (declare (ignore digraph) (ignore current) (ignore goal))
  0)

(defmethod a* ((d digraph) start goal &key (weight #'weight1) (heuristic #'heuristic0) (compare #'>) (adder #'+))
  "The a* search algorithm."
  ;; Input validation
  (check-type heuristic (or keyword function) "a keyword or function")
  (check-type weight (or keyword function) "a keyword or function")
  (unless (node-p d start) (error 'node-not-found-error :node start))
  (unless (node-p d goal) (error 'node-not-found-error :node goal))
  (let* ((open-nodes (make-instance 'pqueue))
         (g-score (make-hash-table :test 'equal))
         (came-from (make-hash-table :test 'equal))
         (closed (make-hash-table :test 'equal)))
    ;; Functions to get weight and heuristic depending on supplied value.
    (labels ((getheuristic
              (node)
              (if (functionp heuristic)
                  (funcall heuristic d node goal)
                  (node-property d node heuristic)))
             (getweight
              (start end)
              (if (functionp weight)
                  (funcall weight d start end)
                  (edge-property d start end weight))))
      ;; Initialize with start node.
      (setf (gethash start g-score) 0)
      (enq open-nodes (getheuristic start) start)
      ;; Basic a* loop.
      (loop while (funcall compare (pqueue-size open-nodes) 0) do
              (let ((cheapest (deq open-nodes)))
                (when (equal cheapest goal)
                      (loop
                     for current = cheapest then (gethash current came-from)
                     while current
                     collect current into path
                     finally (return-from a* (reverse path))))
                (when (gethash cheapest closed)
                      (continue))
                (loop for neighbor in (successor d cheapest) do
                        (let ((tentative-g (funcall adder (gethash cheapest g-score) (getweight cheapest neighbor))))
                          (when (or (not (gethash neighbor g-score))
                                    (< tentative-g (gethash neighbor g-score)))
                                (setf (gethash neighbor came-from) cheapest)
                                (setf (gethash neighbor g-score) tentative-g)
                                (unless (gethash neighbor closed) (enq open-nodes (funcall adder tentative-g (getheuristic neighbor)) neighbor)))))
                (setf (gethash cheapest closed) t))))))
