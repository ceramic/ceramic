(in-package :cl-user)
(defpackage ceramic.util
  (:use :cl)
  (:export :zip-up
           :tar-up
           :without-feature
           :tell)
  (:documentation "Ceramic's utilities."))
(in-package :ceramic.util)

(defun subtract-pathname (root pathname)
  "root is an absolute directory, and pathname is an absolute pathname, such
  that pathname is inside of root. Remove the common directory components,
  leaving a relative pathname."
  (assert (uiop:directory-pathname-p root))
  (assert (uiop:absolute-pathname-p root))
  (assert (uiop:absolute-pathname-p pathname))
  (assert (uiop:subpathp pathname root))
  (uiop:subpathp pathname root))

(defun zip-up (directory output)
  "Create a zip archive from the contents of a directory."
  (zip:with-output-to-zipfile (zipfile output)
    (flet ((write-directory (pathname)
             (zip:write-zipentry zipfile
                                 (namestring
                                  (subtract-pathname directory pathname))
                                 (make-concatenated-stream)
                                 :file-write-date (file-write-date pathname)))
           (write-file (pathname)
             (with-open-file (stream pathname
                                     :element-type '(unsigned-byte 8))
               (zip:write-zipentry zipfile
                                   (namestring
                                    (subtract-pathname directory pathname))
                                   stream
                                   :file-write-date (file-write-date pathname)))))
    (cl-fad:walk-directory directory
                           #'(lambda (pathname)
                               (unless (equal pathname directory)
                                 (if (uiop:directory-pathname-p pathname)
                                     (write-directory pathname)
                                     (write-file pathname))))
                           :directories :breadth-first))))

(defun tar-up (directory output)
  "Create a tar archive from the contents of a directory."
  (let ((*default-pathname-defaults* directory)
        (files (list)))
    (flet ((relativize (pathname)
             (subtract-pathname *default-pathname-defaults* pathname)))
      (cl-fad:walk-directory directory
                             #'(lambda (pathname)
                                 (push (relativize pathname) files))))
    (archive::create-tar-file output files)
    output))

(defmacro tell (format-string &rest args)
  "Log a message."
  `(format t (concatenate 'string "~&" ,format-string)
           ,@args))
