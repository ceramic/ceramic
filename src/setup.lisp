(in-package :cl-user)
(defpackage ceramic.setup
  (:use :cl)
  (:import-from :ceramic.util
                :without-feature
                :tell)
  (:import-from :ceramic.file
                :*ceramic-directory*
                :*buildapp-pathname*)
  (:import-from :ceramic.os
                :*operating-system*
                :*architecture*)
  (:export :setup)
  (:documentation "Set everything up for Ceramic."))
(in-package :ceramic.setup)

(defun install-buildapp ()
  "Install a local copy of Buildapp."
  (tell "Installing Buildap...")
  (if (probe-file *buildapp-pathname*)
      (tell "Already installed")
      (buildapp:build-buildapp (namestring *buildapp-pathname*))))

(defun fix-compiled-libraries ()
  "Recompile some libraries without :quicklisp as a feature."
  (tell "Ensuring some libraries are compiled properly...")
  (with-output-to-string (stream)
    (let ((*standard-output* stream)
          (*error-output* stream))
      (asdf:compile-system :prove :force t)
      (asdf:load-system :clack-v1-compat)))
  nil)

(defun setup ()
  (tell "Creating Ceramic directories...")
  (ensure-directories-exist *ceramic-directory*)
  (install-buildapp)
  (fix-compiled-libraries)
  (tell "Downloading a copy of Electron...")
  (ceramic.electron:setup))
