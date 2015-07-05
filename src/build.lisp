(in-package :cl-user)
(defpackage ceramic.build
  (:use :cl)
  (:import-from :ceramic.file
                :*ceramic-directory*
                :*buildapp-pathname*)
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

(defun build (&key system-name eval output-pathname entry-point)
  "Build an executable from a Lisp system."
  (let ((manifest-pathname (merge-pathnames #p"manifest.txt"
                                            *ceramic-directory*)))
    (with-manifest-file (manifest-pathname)
      (uiop:run-program
       (format nil
               "~S --manifest-file ~S --eval ~S --output ~S --entry ~A --load-system ~A"
               (namestring *buildapp-pathname*)
               (namestring manifest-pathname)
               eval
               (namestring output-pathname)
               entry-point
               (string-downcase (symbol-name system-name)))))))
