(in-package :cl-user)
(defpackage ceramic.file
  (:use :cl)
  (:export :*ceramic-directory*
           :release-directory
           :wipe-data)
  (:documentation "Ceramic's files and directory utilities."))
(in-package :ceramic.file)

(defvar *ceramic-directory*
  (merge-pathnames #p".ceramic/" (user-homedir-pathname))
  "The directory where Ceramic stores its files.")

(defun release-directory ()
  "Pathname to the local copy of the Electron release."
  (merge-pathnames #p"electron/" *ceramic-directory*))

(defun wipe-data ()
  "Wipe all Ceramic related data."
  (uiop:delete-directory-tree *ceramic-directory* :validate t))
