;;;; queue.lisp

(in-package #:lispnet)

(defclass pqueue ()
    ((data :initform (make-array 0 :adjustable t :fill-pointer 0) :accessor pqueue-data)
     (size :initform 0 :accessor pqueue-size)))

(defmethod enq ((p pqueue) key value)
  (if
   (>= (pqueue-size p) (length (pqueue-data p)))
   (vector-push-extend (cons key value) (pqueue-data p))
   (setf (aref (pqueue-data p) (pqueue-size p)) (cons key value)))
  (incf (pqueue-size p))
  (loop for i = (1- (pqueue-size p)) then parent
        for parent = (truncate (1- i) 2)
        while (> i 0) do
          (if
           (< (car (aref (pqueue-data p) i)) (car (aref (pqueue-data p) parent)))
           (rotatef (aref (pqueue-data p) i) (aref (pqueue-data p) parent))
           (return))))

(defmethod deq ((p pqueue))
  (when (<= (pqueue-size p) 0) (error "heap is empty"))
  (let ((root (aref (pqueue-data p) 0)))
    (decf (pqueue-size p))
    (setf (aref (pqueue-data p) 0) (aref (pqueue-data p) (pqueue-size p)))
    (loop for i = 0 then smallest
          for smallest = i do
            (let ((left (+ (* 2 i) 1))
                  (right (+ (* 2 i) 2)))
              (when (and (< left (pqueue-size p))
                         (< (car (aref (pqueue-data p) left)) (car (aref (pqueue-data p) smallest))))
                    (setf smallest left))
              (when (and (< right (pqueue-size p))
                         (< (car (aref (pqueue-data p) right)) (car (aref (pqueue-data p) smallest))))
                    (setf smallest right))
              (if (= i smallest)
                  (return)
                  (progn
                   (rotatef (aref (pqueue-data p) i) (aref (pqueue-data p) smallest))))))
    (cdr root)))