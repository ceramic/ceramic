(in-package :cl-user)
(defpackage ceramic.util
  (:use :cl)
  (:export :copy-directory
           :zip-up)
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
    (make-pathname :directory (if (null position)
                                  nil
                                  (cons :relative
                                        (subseq (pathname-directory pathname)
                                                position)))
                   :defaults pathname)))

(defun copy-directory (source destination)
  "Copy everything under source to destination."
  (ensure-directories-exist destination)
  (fad:walk-directory source
                      #'(lambda (pathname)
                          (unless (equal pathname source)
                            (let* ((relative-path (subtract-pathname source pathname))
                                   (target (merge-pathnames relative-path
                                                            destination)))
                              (if (uiop:directory-pathname-p pathname)
                                  ;; Ensure an equivalent directory exists
                                  (ensure-directories-exist target)
                                  ;; Copy the absolute source file to the target
                                  (uiop:copy-file pathname target)))))
                      :directories :breadth-first)
  destination)

(defun zip-up (directory output)
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
