(in-package :cl-user)
(defpackage ceramic.bundler
  (:use :cl)
  (:import-from :ceramic.log
                :log-message)
  (:import-from :ceramic.file
                :*ceramic-directory*
                :release-directory)
  (:import-from :ceramic.os
                :*operating-system*)
  (:import-from :ceramic.resource
                :copy-resources)
  (:import-from :electron-tools
                :binary-pathname)
  (:export :bundle)
  (:documentation "Release applications."))
(in-package :ceramic.bundler)

(defun archive-extension ()
  "Use zip files on Windows and tar archives on Unix. This is necessary because
tar archives preserve permissions (important for executing!), but Windows
doesn't care about that, and Windows doesn't natively know about tar files (But
most people can unzip)."
  (if (eq *operating-system* :windows)
      "zip"
      "tar"))

(defun create-archive (directory output)
  (case *operating-system*
    (:windows
     (trivial-compress:zip directory output))
    (otherwise
     (trivial-compress:tar directory output))))

(defun bundle (system-name &key bundle-pathname system-directory)
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
                                               work-directory))
         (asdf-registry-prelude
          (if system-directory
              (format nil +asdf-registry-prelude+
                      (uiop:pathname-directory-pathname
                       system-directory)
                      ""))))
    ;; We do everything inside the work directory, then zip it up and delete it
    (ensure-directories-exist work-directory)
    (unwind-protect
         (progn
           (log-message "Copying resources...")
           ;; Copy application resources
           (copy-resources (merge-pathnames #p"resources/"
                                            work-directory))
           ;; Copy the electron directory
           (copy-directory:copy (release-directory)
                                electron-directory)
           ;; Ensure Electron is executable
           (trivial-exe:ensure-executable
            (binary-pathname electron-directory
                             :operating-system *operating-system*))
           ;; Compile the app
           (log-message "Compiling app...")
           (trivial-build:build system-name
                                (format nil "(ceramic-entry::~A)"
                                        (string-downcase
                                         (symbol-name system-name)))
                                executable-pathname)
           ;; Compress the folder
           (when (probe-file bundle-pathname)
             (log-message "Found existing bundle, deleting...")
             (delete-file bundle-pathname))
           (log-message "Compressing...")
           (create-archive work-directory bundle-pathname))
      (uiop:delete-directory-tree work-directory :validate t)
      (log-message "Done!")
      bundle-pathname)))
