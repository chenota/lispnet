;;;; make.lisp

(in-package #:lispnet)

(defun make-digraph ()
  "Create a null graph."
  (make-instance 'digraph))

(defun make-empty (n)
  "Create an empty graph."
  (check-type n (integer 0 *) "a non-negative integer")
  (let
      ((d (make-digraph)))
    (loop for i from 0 below n do (set-node d i))
    d))

(defun make-line (n &optional (bidirectional nil))
  "Create a line."
  (let
      ((d (make-empty n)))
    (loop for i from 1 below n
          do (set-edge d (1- i) i)
            when bidirectional
          do (set-edge d i (1- i)))
    d))

(defun make-ring (n &optional (bidirectional nil))
  "Create a ring."
  (let
      ((d (make-line n bidirectional)))
    (when (> n 1)
          (set-edge d (1- n) 0)
          (when bidirectional (set-edge d 0 (1- n))))
    d))

(defun make-star (n &optional (direction :bi))
  "Create a star graph."
  (check-type direction (member :in :out :bi))
  (let
      ((d (make-empty (1+ n))))
    (loop for i from 1 upto n
            when (member direction '(:out :bi)) do (set-edge d 0 i)
            when (member direction '(:in :bi)) do (set-edge d i 0))
    d))

(defun make-complete (n &optional (allow-self nil))
  "Create a complete graph."
  (let
      ((d (make-empty n)))
    (loop for i from 0 below n
          do (loop for j from 0 below n
                     unless (and (not allow-self) (= i j))
                   do (set-edge d i j)))
    d))

(defun random-bool (p)
  (check-type p (or float rational) "a float or rational")
  (unless (and (>= p 0) (<= p 1)) (error 'probability-out-of-bounds-error :value p))
  (cond
   ((typep p 'float) (< (random 1.0) p))
   ((typep p 'rational) (< (random (denominator p)) (numerator p)))))

(defun make-random (n p &optional (allow-self nil))
  "Create a random graph using the Erdos-Renyi Model"
  (let
      ((d (make-empty n)))
    (loop for i from 0 below n
          do (loop for j from 0 below n
                     unless (and (not allow-self) (= i j))
                     when (random-bool p)
                   do (set-edge d i j)))
    d))