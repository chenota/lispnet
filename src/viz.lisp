;;;; viz.lisp

(in-package #:lispnet)

(defun make-dot-attrs (&rest kvs)
  (loop
 with ht = (make-hash-table)
 for (k v) on kvs by #'cddr
 do (check-type k keyword "a keyword key")
 do (setf (gethash k ht) v)
 finally (return ht)))

(defmethod make-attr-lists (attrs item-count)
  (loop for attr being the hash-keys of attrs
        for attr-val = (gethash attr attrs)
        do (check-type attr keyword "a keyword key")
          if (listp attr-val)
          if (= (length attr-val) item-count)
        collect (mapcar
                    (lambda (x) (format nil "~s=~s" (string-downcase (symbol-name attr)) (princ-to-string x)))
                    attr-val) into vars
          else do (error "invalid attribute count")
          else
        collect (format nil "~s=~s" (string-downcase (symbol-name attr)) (princ-to-string attr-val)) into consts
        finally (return (values consts (if vars (apply #'mapcar #'list vars) nil)))))

(defmethod dot-nodes ((d digraph) attrs)
  (multiple-value-bind
      (const-attrs var-attrs)
      (make-attr-lists attrs (node-count d))
    (with-output-to-string
        (out)
      (if
       var-attrs
       (loop for node being the hash-keys of (slot-value d 'nodes)
             for var-attrs-list in var-attrs
             for attrs-string = (format nil "~{~a~^,~}" (append const-attrs var-attrs-list))
             do (if
                 (string= attrs-string "")
                 (format out "~s;" (princ-to-string node))
                 (format out "~s[~a];" (princ-to-string node) attrs-string)))
       (let
           ((const-attrs-string (format nil "~{~a~^,~}" const-attrs)))
         (if
          (string= const-attrs-string "")
          (loop for node being the hash-keys of (slot-value d 'nodes)
                do (format out "~s;" (princ-to-string node)))
          (loop for node being the hash-keys of (slot-value d 'nodes)
                do (format out "~s[~a];" (princ-to-string node) const-attrs-string))))))))

(defmethod dot-edges ((d digraph) attrs)
  (let ((succ (slot-value d 'succ)))
    (with-output-to-string
        (out)
      (loop for start being the hash-keys of succ do
              (loop for end being the hash-keys of (gethash start succ) do
                      (format out "~s->~s;" (princ-to-string start) (princ-to-string end)))))))

(defmethod dot ((d digraph) &key (node-attrs nil) (edge-attrs nil))
  (concatenate 'string "digraph{" (dot-nodes d node-attrs) (dot-edges d edge-attrs) "}"))