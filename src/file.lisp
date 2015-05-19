(in-package :cl-user)
(defpackage ceramic.file
  (:use :cl)
  (:export :*ceramic-directory*
           :*bundler-directory*
           :*buildapp-pathname*)
  (:documentation "Ceramic's files and directories."))
(in-package :ceramic.file)

(defvar *ceramic-directory*
  (merge-pathnames #p".ceramic"
                   (user-homedir-pathname))
  "The directory where Ceramic stores bundler and user data.")

(defvar *bundler-directory*
  (merge-pathnames #p"bundler/"
                   (user-homedir-pathname))
  "The directory where Ceramic stores files related to the bundler.")

(defvar *buildapp-pathname*
  (merge-pathnames #p"buildapp"
                   *bundler-directory*)
  "The pathname to the local copy of Buildapp.")
