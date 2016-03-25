(in-package :cl-user)
(defpackage ceramic.window
  (:use :cl)
  (:documentation "The window class and its implementation."))
(in-package :ceramic.window)

;;; Classes

(defclass window ()
  ((id :reader window-id
       :initform (uuid:format-as-urn nil (uuid:make-v4-uuid))
       :type string
       :documentation "A unique string ID for the window."))
  (:documentation "A browser window."))

;;; Methods

(defmethod window-title ((window window))
  "Return the window's title.")
