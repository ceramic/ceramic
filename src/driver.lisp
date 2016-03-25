(in-package :cl-user)
(defpackage ceramic.driver
  (:use :cl)
  (:documentation "The Ceramic driver interface."))
(in-package :ceramic.driver)

(defclass driver ()
  ()
  (:documentation "The Ceramic driver."))

(defvar *driver* (make-instance 'driver))
