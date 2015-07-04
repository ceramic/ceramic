(in-package :cl-user)
(defpackage ceramic.util
  (:use :cl)
  (:export :copy-directory)
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
  (let ((position (mismatch (pathname-directory root)
                            (pathname-directory pathname)
                            :test #'equalp)))
    (make-pathname :directory (cons :relative (subseq (pathname-directory pathname)
                                                      position))
                   :defaults pathname)))

(defun copy-directory (source destination)
  "Copy everything under source to destination."
  (ensure-directories-exist destination)
  (fad:walk-directory source
                      #'(lambda (pathname)
                          (let* ((relative-path (subtract-pathname source pathname))
                                 (target (merge-pathnames relative-path
                                                          destination)))
                            (if (uiop:directory-pathname-p pathname)
                                ;; Ensure an equivalent directory exists
                                (ensure-directories-exist target)
                                ;; Copy the absolute source file to the target
                                (uiop:copy-file pathname target))))
                      :directories :breadth-first)
  destination)
