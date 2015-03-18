(in-package :cl-user)
(defpackage ceramic.error
  (:use :cl)
  (:export :ceramic-error
           :unsupported-os)
  (:documentation "Error conditions."))
(in-package :ceramic.error)

(define-condition unsupported-os ()
  ((os-name :reader os-name
            :initarg :os-name
            :type keyword
            :documentation "The name of the operating system."))
  (:report
   (lambda (condition stream)
     (format stream "Unsupported operating system ~A." (os-name condition)))))
