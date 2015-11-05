(in-package :cl-user)
(defpackage ceramic.error
  (:use :cl)
  (:export :ceramic-error
           :unsupported-operating-system
           :not-in-release
           :no-such-tag)
  (:documentation "Error conditions."))
(in-package :ceramic.error)

(define-condition ceramic-error ()
  ()
  (:documentation "The base class of Ceramic-related conditions."))

(define-condition unsupported-operating-system (ceramic-error)
  ((os-name :reader os-name
            :initarg :os-name
            :type keyword
            :documentation "The name of the operating system."))
  (:report
   (lambda (condition stream)
     (format stream "Unsupported operating system ~A." (os-name condition)))))

(define-condition not-in-release (ceramic-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Can't do this, we're not in a release.")))
  (:documentation "Signalled when an operation that requires Ceramic to run in
  release mode is called in interactive mode."))

(define-condition no-such-tag (ceramic-error)
  ((tag :reader error-tag
        :initarg :tag
        :type symbol
        :documentation "The resource tag."))
  (:report
   (lambda (condition stream)
     (format stream "No such resource tag: ~A." (error-tag condition))))
  (:documentation "Signalled when the program references a resource tag that
  doesn't exist."))
