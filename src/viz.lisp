;;;; viz.lisp

(in-package #:lispnet)

(defun make-dot-attrs (&rest kvs)
  (loop
 with ht = (make-hash-table)
 for (k v) on kvs by #'cddr
 do (check-type k keyword "a keyword key")
   (setf
     (gethash (string-downcase (symbol-name k)) ht)
     v)
 finally (return ht)))

(defmethod make-attr-lists (attrs item-count)
  (loop with consts = nil
        with vars = nil
        for attr being the hash-keys of attrs
        for attr-val = (gethash attr attrs)
        do (check-type attr string "a string key")
          if (listp attr-val)
        do (progn
            (unless (= (length attr-val) item-count) (error "invalid attribute count"))
            (push (mapcar (lambda (x) (format nil "~s=~s" attr (princ-to-string x))) attr-val) vars))
          else
        do (push (format nil "~s=~s" attr (princ-to-string attr-val)) consts)
        finally (return (values consts (apply #'mapcar (cons #'list vars))))))

(defmethod dot-nodes ((d digraph) attrs)
  (with-output-to-string (out) (loop for node being the hash-keys of (slot-value d 'nodes) do
                                       (format out "~s;" (princ-to-string node)))))

(defmethod dot-edges ((d digraph) attrs)
  (let ((succ (slot-value d 'succ)))
    (with-output-to-string
        (out)
      (loop for start being the hash-keys of succ do
              (loop for end being the hash-keys of (gethash start succ) do
                      (format out "~s->~s;" (princ-to-string start) (princ-to-string end)))))))

(defmethod dot ((d digraph) &key (node-attrs nil) (edge-attrs nil))
  (concatenate 'string "digraph{" (dot-nodes d node-attrs) (dot-edges d edge-attrs) "}"))