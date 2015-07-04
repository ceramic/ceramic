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
  #+sbcl
  sb-ext:*runtime-pathname*
  #+ccl
  (truename (make-pathname :host "ccl"))
  #+ecl
  (truename (make-pathname :host "sys"))
  #-(or sbcl ccl ecl)
  (if (probe-file #p"/proc/self/exe")
      ;; Linux
      (uiop:resolve-absolute-location #p"/proc/self/exe")
      ;; FreeBSD
      (uiop:resolve-absolute-location #p"/proc/curproc/file")))

(defun executable-relative-pathname (pathname)
  "Return an absolute pathname relative to the executable pathname."
  (merge-pathnames pathname (executable-pathname)))
