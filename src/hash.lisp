;;;; hash.lisp

(in-package #:lispnet)

(defun gethash-or (key table default)
  (multiple-value-bind
      (val exists)
      (gethash key table)
    (if
     exists
     val
     (setf (gethash key table) default))))

(defun gethash-multi (table &rest args)
  (labels
      ((gethash-multi-helper
        (keys current exists-path)
        (cond
         ((not keys)
           (values current exists-path))
         ((not (typep current 'hash-table))
           (gethash-multi-helper (cdr keys) nil (cons nil exists-path)))
         (t
           (multiple-value-bind (next exists)
               (gethash (car keys) current)
             (gethash-multi-helper (cdr keys) next (cons exists exists-path)))))))
    (gethash-multi-helper args table '())))