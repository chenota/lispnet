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

(defmethod a* ((d digraph) start goal &key (weight #'weight1) (heuristic #'heuristic0) (memoize-weight nil) (memoize-heuristic nil))
  "The a* search algorithm."
  (let* ((open-nodes (make-instance 'pqueue))
         (g-score (make-hash-table :test 'equal))
         (weight-table (make-hash-table :test 'equal))
         (heuristic-table (make-hash-table :test 'equal))
         (came-from (make-hash-table :test 'equal))
         (closed (make-hash-table :test 'equal)))
    ;; Functions to memoize values if user opts to.
    (labels ((memo-heuristic
              (node)
              (if memoize-heuristic
                  (multiple-value-bind (val ok) (gethash node heuristic-table)
                    (if ok val
                        (progn
                         (setf (gethash node heuristic-table) (funcall heuristic d node goal))
                         (gethash node heuristic-table))))
                  (funcall heuristic d node goal)))
             (memo-weight
              (start end)
              (if memoize-weight
                  (multiple-value-bind (val ok) (gethash (cons start end) weight-table)
                    (if ok val
                        (progn
                         (setf (gethash (cons start end) weight-table) (funcall weight d start end))
                         (gethash (cons start end) weight-table))))
                  (funcall weight d start end))))
      ;; Initialize with start node.
      (setf (gethash start g-score) 0)
      (enq open-nodes (memo-heuristic start) start)
      ;; Basic a* loop.
      (loop while (> (pqueue-size open-nodes) 0) do
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
                        (let ((tentative-g (+ (gethash cheapest g-score) (memo-weight cheapest neighbor))))
                          (when (or (not (gethash neighbor g-score))
                                    (< tentative-g (gethash neighbor g-score)))
                                (setf (gethash neighbor came-from) cheapest)
                                (setf (gethash neighbor g-score) tentative-g)
                                (enq open-nodes (+ tentative-g (memo-heuristic neighbor)) neighbor))))
                (setf (gethash cheapest closed) t)))))
  nil)
