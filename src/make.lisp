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

(defun make-line (n &key (bi nil))
  "Create a line."
  (let
      ((d (make-empty n)))
    (loop for i from 1 below n
          do (set-edge d (1- i) i)
            when bi do (set-edge d i (1- i)))
    d))

(defun make-ring (n &key (bi nil))
  "Create a ring."
  (let
      ((d (make-line n :bi bi)))
    (when (> n 1)
          (set-edge d (1- n) 0)
          (when bi (set-edge d 0 (1- n))))
    d))

(defun make-ring-lattice (n k &key (bi nil))
  "Create a ring lattice."
  (check-type k (integer 0 *) "a non-negative integer")
  (unless (evenp k) (error 'uneven-arg :source "k"))
  (if
   (evenp n)
   (unless (<= k (- n 2)) (error 'excessive-arg :source "k" :target "n-2"))
   (unless (<= k (- n 1)) (error 'excessive-arg :source "k" :target "n-1")))
  (let
      ((d (make-empty n)))
    (loop for i from 0 below n
          do (loop for j from 1 upto (/ k 2)
                   do (set-edge d i (mod (+ i j) n))
                   do (set-edge d (mod (- i j) n) i)
                     when bi do (set-edge d (mod (+ i j) n) i)
                     when bi do (set-edge d i (mod (- i j) n))))
    d))

(defun make-star (n &key (direction :bi))
  "Create a star graph."
  (check-type direction (member :in :out :bi))
  (let
      ((d (make-empty (1+ n))))
    (loop for i from 1 upto n
            when (member direction '(:out :bi)) do (set-edge d 0 i)
            when (member direction '(:in :bi)) do (set-edge d i 0))
    d))

(defun make-complete (n &key (allow-self nil))
  "Create a complete graph."
  (let
      ((d (make-empty n)))
    (loop for i from 0 below n
          do (loop for j from 0 below n
                     unless (and (not allow-self) (= i j))
                   do (set-edge d i j)))
    d))

(defun make-havel-hakimi (seq &key (direction :bi))
  "Create a simple graph using the Havel-Hakimi algorithm."
  (check-type seq sequence)
  (loop for v in seq do (check-type v (integer 0 *) "a non-negative integer"))
  (check-type direction (member :in :out :bi))
  (let
      ((d (make-empty (length seq))))
    (loop for l = (sort (loop for i from 0 for v in seq collect (cons v i)) #'> :key #'car) then (cdr l)
          while l
          do (loop
            with count = 0
            for ll = (cdr l) then (cdr ll)
            while (< count (caar l))
              when (null ll) do (error 'not-graphical)
              when (> (caar ll) 0)
            do (progn
                (incf count)
                (decf (caar ll))
                (when (member direction '(:out :bi)) (set-edge d (cdar l) (cdar ll)))
                (when (member direction '(:in :bi)) (set-edge d (cdar ll) (cdar l))))))
    d))

(defun random-bool (p)
  (check-type p (or float rational) "a float or rational")
  (unless (and (>= p 0) (<= p 1)) (error 'probability-out-of-bounds-error :value p))
  (cond
   ((typep p 'float) (< (random 1.0) p))
   ((typep p 'rational) (< (random (denominator p)) (numerator p)))))

(defun make-random (n p &key (allow-self nil))
  "Create a random graph using the Erdos-Renyi model."
  (let
      ((d (make-empty n)))
    (loop for i from 0 below n
          do (loop for j from 0 below n
                     unless (and (not allow-self) (= i j))
                     when (random-bool p)
                   do (set-edge d i j)))
    d))

(defun barabasi-albert (d node)
  (if (> (edge-count d) 0)
      (/ (degree d node) (edge-count d))
      1.0))

(defun make-preferential (n &key (n0 0) (direction :bi) (p #'barabasi-albert))
  "Make a random graph using the preferential attachment model."
  (check-type direction (member :in :out :bi))
  (check-type n (integer 0 *) "a non-negative integer")
  (check-type n0 (integer 0 *) "a non-negative integer")
  (when (> n0 n) (error 'excessive-arg :source "m0" :target "m"))
  (let
      ((d (make-complete n0)))
    (loop for i from n0 below n
          do (set-node d i)
          do (loop for j from 0 below n0
                     when (random-bool (funcall p d j))
                   do (progn
                       (when (member direction '(:out :bi)) (set-edge d j i))
                       (when (member direction '(:in :bi)) (set-edge d i j)))))
    d))