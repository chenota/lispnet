;;;; viz.lisp

(in-package #:lispnet)

(defun escape-quotes)

(defmethod dot-nodes ((d digraph))
  (with-output-to-string (out) (loop for node being the hash-keys of (slot-value d 'nodes) do
                                       (format out "~s;" (princ-to-string node)))))

(defmethod dot-edges ((d digraph))
  (let ((succ (slot-value d 'succ))) z
    (with-output-to-string
        (out)
      (loop for start being the hash-keys of succ do
              (loop for end being the hash-keys of (gethash start succ) do
                      (format out "~s->~s;" (princ-to-string start) (princ-to-string end)))))))

(defmethod dot ((d digraph))
  (concatenate 'string "digraph{" (dot-nodes d) (dot-edges d) "}"))