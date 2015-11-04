(in-package :cl-user)
(defpackage ceramic.runtime
  (:use :cl)
  (:import-from :ceramic.error
                :not-in-release)
  (:export :*releasep*
           :executable-pathname
           :executable-relative-pathname)
  (:documentation "Tools for execution during release."))
(in-package :ceramic.runtime)

(defvar *releasep* nil
  "Dynamically bound to t when in release mode.")

(defun executable-pathname ()
  "Return the executable's pathname."
  (unless *releasep*
    (error 'not-in-release))
  (trivial-exe:executable-pathname))

(defun executable-relative-pathname (pathname)
  "Return an absolute pathname relative to the executable pathname."
  (merge-pathnames pathname (executable-pathname)))
