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
