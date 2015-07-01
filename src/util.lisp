(in-package :cl-user)
(defpackage ceramic.util
  (:use :cl)
  (:export :executable-path)
  (:documentation "Ceramic's utilities."))
(in-package :ceramic.util)

(defun executable-path ()
  "Return the executable's pathname."
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
