(in-package :cl-user)
(defpackage ceramic.os
  (:use :cl)
  (:import-from :ceramic.error
                :unsupported-operating-system)
  (:export :*operating-system*
           :*architecture*)
  (:documentation "Operating system-related stuff."))
(in-package :ceramic.os)

(defun detect-operating-system ()
  (cond
    ((uiop:os-windows-p)
     :windows)
    ((uiop:os-macosx-p)
     :mac)
    ((equal (uiop:operating-system) :linux) ;;but not BSDs and other *nix
     :linux)
    (t
     (error 'unsupported-operating-system
            :os-name (uiop:operating-system)))))

(defun detect-architecture ()
  (if (eql (cffi:foreign-type-size '(:pointer :int)) 8)
      :64
      :32))

(defvar *operating-system* (detect-operating-system)
  "The operating system. Either @c(:linux), @c(:windows) or @c(:mac).")

(defvar *architecture* (detect-architecture)
  "The OS architecture. Either @c(:64) or @c(:32).")
