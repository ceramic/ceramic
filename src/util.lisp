(in-package :cl-user)
(defpackage ceramic.util
  (:use :cl)
  (:export :without-feature
           :tell)
  (:documentation "Ceramic's utilities."))
(in-package :ceramic.util)

(defmacro tell (format-string &rest args)
  "Log a message."
  `(format t (concatenate 'string "~&" ,format-string)
           ,@args))
