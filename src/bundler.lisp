(in-package :cl-user)
(defpackage ceramic.bundler
  (:use :cl)
  (:import-from :ceramic.util
                :zip-up
                :tar-up
                :copy-directory
                :ensure-executable
                :tell)
  (:import-from :ceramic.file
                :*ceramic-directory*)
  (:import-from :ceramic.os
                :*operating-system*)
  (:import-from :ceramic.resource
                :copy-resources)
  (:import-from :ceramic.electron.tools
                :binary-pathname)
  (:export :bundle)
  (:documentation "Release applications."))
(in-package :ceramic.bundler)

(defvar +prelude+
  "(progn (push :ceramic-release *features*))")

(defun archive-extension ()
  "Use zip files on Windows and tar archives on Unix. This is necessary because
tar archives preserve permissions (important for executing!), but Windows
doesn't care about that, and Windows doesn't natively know about tar files (But
most people can unzip)."
  (if (eq *operating-system* :windows)
      "zip"
      "tar"))

(defun create-archive (directory output)
  (if (eq *operating-system* :windows)
      (zip-up directory output)
      (tar-up directory output)))

(defun bundle (system-name &key bundle-pathname)
  "Compile the application to an executable, and ship it with its resources."
  (asdf:load-system system-name)
  (let* ((application-name (string-downcase
                            (symbol-name system-name)))
         (bundle (make-pathname :name application-name
                                :type (archive-extension)))
         (bundle-pathname (or bundle-pathname
                              (asdf:system-relative-pathname system-name
                                                             bundle)))
         (work-directory (merge-pathnames #p"working/"
                                          *ceramic-directory*))
         (electron-directory (merge-pathnames #p"electron/"
                                              work-directory))
         (executable-pathname (merge-pathnames (make-pathname :name application-name)
                                               work-directory)))
    ;; We do everything inside the work directory, then zip it up and delete it
    (ensure-directories-exist work-directory)
    (unwind-protect
         (progn
           (tell "Copying resources...")
           ;; Copy application resources
           (copy-resources (merge-pathnames #p"resources/"
                                            work-directory))
           ;; Copy the electron directory
           ;; (copy-directory (ceramic.electron:release-directory)
           ;;                 electron-directory)
           ;; ;; Ensure Electron is executable
           ;; (ensure-executable
           ;;  (binary-pathname electron-directory
           ;;                   :operating-system *operating-system*))
           ;; Compile the app
           (tell "Compiling app...")
           (ceramic.build:build :eval +prelude+
                                :system-name system-name
                                :output-pathname executable-pathname
                                :entry-point (concatenate 'string
                                                          "ceramic-entry::"
                                                          (string-downcase
                                                           (symbol-name system-name))))
           ;; Compress the folder
           (when (probe-file bundle-pathname)
             (tell "Found existing bundle, deleting...")
             (delete-file bundle-pathname))
           (tell "Compressing...")
           ;; (create-archive work-directory bundle-pathname)
	   )
      ;; (Uiop:delete-directory-tree work-directory :validate t)
      (tell "Done!")
      bundle-pathname)))
