;;;; viz.lisp

(in-package #:lispnet)

(defmethod dot-nodes ((d digraph) attrs)
  "Generate node section of a dotfile."
  (with-output-to-string
      (out)
    (loop for node in (nodes d)
          for attrs-string = (format
                                 nil
                                 "~{~a~^,~}"
                               (loop for (attr func) on attrs by #'cddr collect
                                       (format nil "~s=~s" (string-downcase (symbol-name attr)) (funcall func d node))))
          do (if
              (string= attrs-string "")
              (format out "~s;" (princ-to-string node))
              (format out "~s[~a];" (princ-to-string node) attrs-string)))))

(defmethod dot-edges ((d digraph) attrs)
  "Generate edges section of a dotfile."
  (with-output-to-string
      (out)
    (loop for (start . end) in (edges d)
          for attrs-string = (format
                                 nil
                                 "~{~a~^,~}"
                               (loop for (attr func) on attrs by #'cddr collect
                                       (format nil "~s=~s" (string-downcase (symbol-name attr)) (funcall func d start end))))
          do (if
              (string= attrs-string "")
              (format out "~s->~s;" start end)
              (format out "~s->~s[~a];" start end attrs-string)))))

(defmethod dot ((d digraph) &key (node-attrs nil) (edge-attrs nil))
  "Generate a dotfile as a string."
  (concatenate 'string "digraph{" (dot-nodes d node-attrs) (dot-edges d edge-attrs) "}"))