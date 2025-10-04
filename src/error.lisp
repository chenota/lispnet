;;;; error.lisp

(in-package #:lispnet)

(define-condition lispnet-error (error)
    ((message :initarg :message :initform "error"))
  (:report (lambda (c s)
             ;; Figure out the non-message slots that belong to the error
             (let ((slot-names
                    (remove-if
                        (lambda (slot) (or (eq slot 'message) (not (slot-boundp c slot))))
                        (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of c))))))
               ;; Print handy error message
               (format s "lispnet: ~a" (slot-value c 'message))
               (when slot-names
                     (format s " [~{~a~^,~}]"
                       (mapcar
                           (lambda (name) (format nil "~a=~a" (string-downcase (symbol-name name)) (slot-value c name)))
                           slot-names)))))))

(define-condition not-found-error (lispnet-error)
    ()
  (:default-initargs :message "not found"))

(define-condition node-not-found-error (not-found-error)
    ((node :initarg :node))
  (:default-initargs :message "node not found"))

(define-condition property-not-found-error (not-found-error)
    ((property :initarg :prop))
  (:default-initargs :message "property not found"))

(define-condition edge-not-found-error (not-found-error)
    ((begin :initarg :begin)
     (end :initarg :end))
  (:default-initargs :message "edge not found"))

(define-condition uneven-argument-error (lispnet-error)
    ()
  (:default-initargs :message "expected an even number of arguments"))

(define-condition nested-hash-not-found-error (not-found-error)
    ((level :initarg :level :reader failure-level))
  (:default-initargs :message "nested hash value not found"))

(define-condition out-of-bounds-error (lispnet-error)
    ()
  (:default-initargs :message "out of bounds"))

(define-condition probability-out-of-bounds-error (out-of-bounds-error)
    ((value :initarg :value))
  (:default-initargs :message "probability not in range [0, 1]"))

(define-condition invalid-sequence (lispnet-error)
    ()
  (:default-initargs :message "invalid sequence"))

(define-condition not-graphical (invalid-sequence)
    ()
  (:default-initargs :message "sequence is not graphical"))