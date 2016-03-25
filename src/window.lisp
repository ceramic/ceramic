(in-package :cl-user)
(defpackage ceramic.window
  (:use :cl)
  (:import-from :ceramic.driver
                :*driver*)
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

(defun js (format-control &rest args)
  (ceramic.driver:sync-js *driver* (apply #'format (cons nil (cons format-control args)))))

(defmethod window-title ((window window))
  "Return the window's title."
  (with-slots (id) window
    (js "Ceramic.windows[~S].getTitle()" id)))

(defmethod (setf window-title) (new-value ((window window)))
  "Set the window's title."
  (with-slots (id) window
    (js "Ceramic.windows[~S].setTitle(~S)" id new-value)))

(defmethod center ((window window))
  "Move the window to the center of the screen."
  (with-slots (id) window
    (js "Ceramic.windows[~S].center()" id)))
