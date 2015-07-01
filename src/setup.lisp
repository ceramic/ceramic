(in-package :cl-user)
(defpackage ceramic.setup
  (:use :cl)
  (:import-from :ceramic.file
                :*ceramic-directory*
                :*buildapp-pathname*)
  (:import-from :ceramic.os
                :*operating-system*
                :*architecture*)
  (:export :setup)
  (:documentation "Set everything up for Ceramic."))
(in-package :ceramic.setup)

(defun install-buildapp ()
  "Install a local copy of Buildapp."
  (buildapp:build-buildapp (namestring *buildapp-pathname*)))

(defun setup ()
  "Set up everything needed for Ceramic to run."
  (ensure-directories-exist *ceramic-directory*)
  (install-buildapp)
  (ceramic.electron:setup))
