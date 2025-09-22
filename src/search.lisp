;;;; search.lisp

(in-package #:lispnet)

(defmethod a* ((d digraph) start goal weight heuristic)
  (let* ((open-nodes (make-instance 'pqueue))
         (g-score (make-hash-table :test 'equal))
         (came-from (make-hash-table :test 'equal))
         (closed (make-hash-table :test 'equal)))
    (setf (gethash start g-score) 0)
    (let ((h (funcall heuristic d start goal)))
      (enq open-nodes h start))
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
                      (let ((tentative-g (+ (gethash cheapest g-score) (funcall weight d cheapest neighbor))))
                        (when (or (not (gethash neighbor g-score))
                                  (< tentative-g (gethash neighbor g-score)))
                              (setf (gethash neighbor came-from) cheapest)
                              (setf (gethash neighbor g-score) tentative-g)
                              (enq open-nodes (+ tentative-g (funcall heuristic d neighbor goal)) neighbor))))
              (setf (gethash cheapest closed) t))))
  nil)
