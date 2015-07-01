(in-package :cl-user)
(defpackage ceramic.build
  (:use :cl)
  (:import-from :ceramic.file
                :*buildapp-pathname*)
  (:export :build)
  (:documentation "A Buildapp interface."))
(in-package :ceramic.build)

(defun build (system-name output-pathname entry-point)
  "Build an executable from a Lisp system."
  (uiop:run-program (format nil "~S --output ~S --entry ~A --load-system ~A"
                            (namestring *buildapp-pathname*)
                            (namestring output-pathname)
                            entry-point
                            system-name)))
