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

(defmacro window-js (id &rest args)
  `(js "Ceramic.windows[~S].~A" ,id ,@args))

(defmethod window-title ((window window))
  "Return the window's title."
  (with-slots (id) window
    (window-js "getTitle()" id)))

(defmethod (setf window-title) (new-value (window window))
  "Set the window's title."
  (with-slots (id) window
    (window-js "setTitle(~S)" id new-value)))

(defmethod window-url ((window window))
  "Return the window's current URL."
  (with-slots (id) window
    (window-js "webContents.getURL()" id)))

(defmethod (setf window-url) (new-value (window window))
  "Change the window's URL."
  (with-slots (id) window
    (window-js "loadURL(~S") id new-value))

(defmethod center ((window window))
  "Move the window to the center of the screen."
  (with-slots (id) window
    (window-js "center()" id)))

(defmethod reload ((window window))
  "Reload the window."
  (with-slots (id) window
    (window-js "reload()" id)))
