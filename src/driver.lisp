(in-package :cl-user)
(defpackage ceramic.driver
  (:use :cl)
  (:documentation "The Ceramic driver interface."))
(in-package :ceramic.driver)

(defclass driver ()
  ((process :accessor driver-process
            :documentation "The Electron process.")
   (context :accessor driver-context
            :type remote-js:buffered-context
            :documentation "The remote-js object."))
  (:documentation "The Ceramic driver."))

(defvar *driver* (make-instance 'driver)
  "The global driver object.")

(defmethod initialize-instance :after ((driver driver) &key)
  (setf (driver-context driver)
        (remote-js:make-buffered-context :callback
                                         #'(lambda (message)
                                             (on-message driver message)))))

(defgeneric on-message (driver message)
  (:documentation "React to a message from a WebSockets client."))
