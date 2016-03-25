(in-package :cl-user)
(defpackage ceramic.setup
  (:use :cl)
  (:import-from :ceramic.log
                :log-message)
  (:import-from :ceramic.file
                :*ceramic-directory*)
  (:import-from :ceramic.os
                :*operating-system*
                :*architecture*)
  (:export :setup)
  (:documentation "Set everything up for Ceramic."))
(in-package :ceramic.setup)

(defun setup ()
  (log-message "Creating Ceramic directories...")
  (ensure-directories-exist *ceramic-directory*)
  (log-message "Downloading a copy of Electron...")
  (ceramic.electron:setup))
