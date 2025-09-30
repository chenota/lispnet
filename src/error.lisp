;;;; error.lisp

(in-package #:lispnet)

(define-condition lispnet-error (error)
    ((message :initform "error"))
  (:report (lambda (c s)
             ;; Figure out the non-message slots that belong to the error
             (let ((slot-names
                    (remove-if
                        (lambda (slot) (or (eq slot 'message) (null (slot-boundp c slot))))
                        (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of c))))))
               ;; Print handy error message
               (if slot-names
                   (format s "lispnet: ~a [~{~a~^,~}]"
                     (slot-value c 'message)
                     (mapcar
                         (lambda (name) (format nil "~a=~a" (string-downcase (symbol-name name)) (slot-value c name)))
                         slot-names))
                   (format s "lispnet: ~a" (slot-value c 'message)))))))

(define-condition not-found-error (lispnet-error)
    ((message :initform "not found")))

(define-condition node-not-found-error (not-found-error)
    ((node :initarg :node)
     (message :initform "node not found")))

(define-condition property-not-found-error (not-found-error)
    ((property :initarg :prop)
     (message :initform "property not found")))

(define-condition edge-not-found-error (not-found-error)
    ((begin :initarg :begin)
     (end :initarg :end)
     (message :initform "edge not found")))

(define-condition uneven-argument-error (lispnet-error)
    ((message :initform "expected an even number of arguments")))

(define-condition nested-hash-not-found-error (not-found-error)
    ((message :initform "nested hash value not found")
     (level :initarg :level :reader failure-level)))

(define-condition out-of-bounds-error (lispnet-error)
    ((message :initform "out of bounds")))

(define-condition probability-out-of-bounds-error (lispnet-error)
    ((message :initform "probability not in range [0, 1]")
     (value :initarg :value)))