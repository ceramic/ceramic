(in-package :cl-user)
(defpackage ceramic.os
  (:use :cl)
  (:export :operating-system
           :linux
           :linux-32
           :linux-64
           :windows
           :mac
           :current-os)
  (:documentation "Operating system classes for Ceramic."))
(in-package :ceramic.os)

(defclass operating-system ()
  ()
  (:documentation "The base class for an operating system."))

(defclass linux (operating-system)
  ()
  (:documentation "Generic Linux."))

(defclass linux-32 (linux)
  ()
  (:documentation "32-bit Linux."))

(defclass linux-64 (linux)
  ()
  (:documentation "64-bit Linux."))

(defclass windows (operating-system)
  ()
  (:documentation "Windows."))

(defclass mac (operating-system)
  ()
  (:documentation "Mac."))

(defun current-os ()
  "Return the current operating system, an instance of operating-system."
  (let ((64-bit (eql (cffi:foreign-type-size '(:pointer :int)) 8)))
    (cond
      ((uiop:os-windows-p)
       (make-instance 'windows))
      ((uiop:os-macosx-p)
       (make-instance 'mac))
      ((eq (uiop:operating-system) :linux)
       (if 64-bit
           (make-instance 'linux-64)
           (make-instance 'linux-32)))
      (t
       (error 'ceramic.error:unsupported-os
              :os-name (uiop:operating-system))))))
