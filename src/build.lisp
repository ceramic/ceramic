(in-package :cl-user)
(defpackage ceramic.build
  (:use :cl)
  (:import-from :ceramic.file
                :*ceramic-directory*
                :*buildapp-pathname*)
  (:import-from :ceramic.error
                :compilation-error)
  (:export :build)
  (:documentation "A Buildapp interface."))
(in-package :ceramic.build)

(defmacro with-manifest-file ((pathname) &body body)
  "Create an ASDF manifest file and delete it when we're done."
  `(progn
     (ql:write-asdf-manifest-file ,pathname)
     (unwind-protect
          (progn ,@body)
       (delete-file ,pathname))))

(defparameter +command-format+
  "~S --eval ~S --manifest-file ~S --load-system ~A --output ~S --entry ~A")

(defun build (&key eval system-name output-pathname entry-point)
  "Build an executable from a Lisp system."
  (let* ((manifest-pathname (merge-pathnames #p"manifest.txt"
                                            *ceramic-directory*))
         (command (format nil
                          +command-format+
                          (namestring *buildapp-pathname*)
                          eval
                          (namestring manifest-pathname)
                          (string-downcase (symbol-name system-name))
                          (namestring output-pathname)
                          entry-point)))
    (with-manifest-file (manifest-pathname)
      (multiple-value-bind (output stderr status-code)
          (uiop:run-program command
                            :output :string
                            :error-output :output
                            :ignore-error-status t)
        (declare (ignore stderr))
        (when (not (= status-code 0))
          (error 'compilation-error
                 :command command
                 :message output))))))
