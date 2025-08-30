;;;; viz.lisp

(in-package #:lispnet)

(defmethod make-attr-lists (attrs item-count)
  (let ((attrs-hash
         (loop
        with ht = (make-hash-table)
        for (k v) on attrs by #'cddr
        do (check-type k keyword "a keyword key")
        do (setf (gethash k ht) v)
        finally (return ht))))
    (loop for attr being the hash-keys of attrs-hash
          for attr-val = (gethash attr attrs-hash)
            if (listp attr-val)
            if (= (length attr-val) item-count)
          collect (mapcar
                      (lambda (x) (format nil "~s=~s" (string-downcase (symbol-name attr)) (princ-to-string x)))
                      attr-val) into vars
            else do (error "invalid attribute count")
            else
          collect (format nil "~s=~s" (string-downcase (symbol-name attr)) (princ-to-string attr-val)) into consts
          finally (return (values consts (if vars (apply #'mapcar #'list vars) nil))))))

(defmethod dot-nodes ((d digraph) attrs)
  (multiple-value-bind
      (const-attrs var-attrs)
      (make-attr-lists attrs (node-count d))
    (with-output-to-string
        (out)
      (if
       var-attrs
       (loop for node in (nodes d)
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
          (loop for node in (nodes d)
                do (format out "~s;" (princ-to-string node)))
          (loop for node in (nodes d)
                do (format out "~s[~a];" (princ-to-string node) const-attrs-string))))))))

(defmethod dot-edges ((d digraph) attrs)
  (multiple-value-bind
      (const-attrs var-attrs)
      (make-attr-lists attrs (edge-count d))
    (with-output-to-string
        (out)
      (if
       var-attrs
       (loop for edge in (edges d)
             for var-attrs-list in var-attrs
             for attrs-string = (format nil "~{~a~^,~}" (append const-attrs var-attrs-list))
             do (if
                 (string= attrs-string "")
                 (format out "~s->~s;" (princ-to-string (car edge)) (princ-to-string (cdr edge)))
                 (format out "~s->~s[~a];" (princ-to-string (car edge)) (princ-to-string (cdr edge)) attrs-string)))
       (let
           ((const-attrs-string (format nil "~{~a~^,~}" const-attrs)))
         (if
          (string= const-attrs-string "")
          (loop for edge in (edges d)
                do (format out "~s->~s;" (princ-to-string (car edge)) (princ-to-string (cdr edge))))
          (loop for edge in (edges d)
                do (format out "~s->~s[~a];" (princ-to-string (car edge)) (princ-to-string (cdr edge)) const-attrs-string))))))))

(defmethod dot ((d digraph) &key (node-attrs nil) (edge-attrs nil))
  (concatenate 'string "digraph{" (dot-nodes d node-attrs) (dot-edges d edge-attrs) "}"))