(in-package :cl-user)
(defpackage ceramic.setup
  (:use :cl)
  (:import-from :ceramic.util
                :tell)
  (:import-from :ceramic.file
                :*ceramic-directory*)
  (:import-from :ceramic.os
                :*operating-system*
                :*architecture*)
  (:export :setup)
  (:documentation "Set everything up for Ceramic."))
(in-package :ceramic.setup)

(defun setup ()
  (tell "Creating Ceramic directories...")
  (ensure-directories-exist *ceramic-directory*)
  (tell "Downloading a copy of Electron...")
  (ceramic.electron:setup))
