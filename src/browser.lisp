(in-package :cl-user)
(defpackage ceramic.browser
  (:use :cl)
  (:import-from :ceramic.os
                :linux
                :windows
                :mac)
  (:export :run-browser-command)
  (:documentation "Run Chromium instances."))
(in-package :ceramic.browser)

(defgeneric run-browser-command (os binary-pathname args)
  (:documentation ""))

(defmethod run-browser-command :before (os binary-pathname args)
  (check-type binary-pathname pathname)
  (check-type args list))

(defmethod run-browser-command ((os linux) binary-pathname args)
  (format nil "~S --no-sandbox ~{~A ~}"
          (namestring binary-pathname)
          args))

(defmethod run-browser-command ((os windows) binary-pathname args)
  (format nil "~S ~{~A ~}"
          (namestring binary-pathname)
          args))

(defmethod run-browser-command ((os mac) binary-pathname args)
  (format nil "open -n ~S --args ~{~A ~}"
          (namestring binary-pathname)
          args))

(defun launch-browser (os binary-pathname url)
  "Lauch a browser window to the specified URL, then return the browser process."
  (let* ((args (list (format nil "-app=~S" url)))
         (command (run-browser-command os binary-pathname args)))
    (external-program:start command (list))))
