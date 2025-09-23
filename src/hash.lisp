;;;; hash.lisp

(in-package #:lispnet)

(defun gethash-or (key table default)
  "Get the stored hash value or a provided default if the value doesn't exist. Adds the provided default as a value to the table."
  (multiple-value-bind
      (val exists)
      (gethash key table)
    (if
     exists
     val
     (setf (gethash key table) default))))

(defun gethash-multi (table &rest args)
  "Traverse nested hash tables to retrieve a value."
  (loop with current = table
        for arg in args
        for level = 0 then (1+ level)
        do (unless (typep table 'hash-table) (error 'nested-hash-not-found-error :level level))
        do (setf
             current
             (multiple-value-bind
                 (value found)
                 (gethash arg current)
               (unless found (error 'nested-hash-not-found-error :level level))
               value))
        finally (return current)))