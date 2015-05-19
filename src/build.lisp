(in-package :cl-user)
(defpackage ceramic.build
  (:use :cl)
  (:import-from :ceramic.file
                :*buildapp-pathname*)
  (:export :build)
  (:documentation "A Buildapp interface."))
(in-package :ceramic.build)

(defun install-buildapp ()
  "Install a local copy of Buildapp."
  (buildapp:build-buildapp (namestring *buildapp-pathname*)))

(defun ensure-buildapp-is-installed ()
  "Ensure Buildapp is installed."
  (unless (probe-file *buildapp-pathname*)
    (install-buildapp)))

(defun build (system-name output-pathname entry-point)
  "Build an executable from a Lisp system."
  (ensure-buildapp-is-installed)
  (uiop:run-program (format nil "~S --output ~S --entry ~A --load-system ~A"
                            (namestring *buildapp-pathname*)
                            (namestring output-pathname)
                            entry-point
                            system-name)))
