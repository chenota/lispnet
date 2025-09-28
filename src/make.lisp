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

(defun make-di-line (n)
  "Create a directed line."
  (let
      ((d (make-empty n)))
    (loop for i from 1 below n do (set-edge d (1- i) i))
    d))

(defun make-bi-line (n)
  "Create a bidirectional line."
  (let
      ((d (make-empty n)))
    (loop for i from 1 below n do (set-edge d (1- i) i) do (set-edge d i (1- i)))
    d))

(defun make-di-ring (n)
  "Create a directed ring."
  (let
      ((d (make-di-line n)))
    (when (> n 1) (set-edge d (1- n) 0))
    d))

(defun make-bi-ring (n)
  "Create a bidirectional ring."
  (let
      ((d (make-bi-line n)))
    (when (> n 1) (set-edge d (1- n) 0) (set-edge d 0 (1- n)))
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

(defun make-complete (n)
  "Create a complete graph."
  (let
      ((d (make-empty n)))
    (loop for i from 0 below n
          do (loop for j from 0 below n
                     unless (= i j)
                   do (set-edge d i j)))
    d))