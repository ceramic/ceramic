(in-package :cl-user)
(defpackage ceramic.setup
  (:use :cl)
  (:import-from :ceramic.log
                :log-message)
  (:import-from :ceramic.file
                :*ceramic-directory*
                :release-directory)
  (:import-from :ceramic.os
                :*operating-system*
                :*architecture*)
  (:import-from :electron-tools
                :app-directory
                :binary-pathname
                :get-release)
  (:export :setup
           :global-binary-pathname)
  (:documentation "Set up everything needed to develop with Ceramic."))
(in-package :ceramic.setup)

;;; Dealing with Electron releases

(defparameter +main-javascript+
  (asdf:system-relative-pathname :ceramic #p"src/electron/main.js")
  "Pathname to the JavaScript file for the main process.")

(defun clean-release (directory &key operating-system)
  "Clean up default files from an Electron release."
  (let ((app-files (list #p"main.js"
                         #p"default_app.js"
                         #p"index.html"
                         #p"package.json")))
    (loop for file in app-files do
      (let ((pathname (merge-pathnames file
                                       (app-directory directory :operating-system operating-system))))
        (delete-file pathname)))))

(defun insert-javascript (directory &key operating-system)
  "Insert the main process JavaScript into an Electron release."
  (uiop:copy-file +main-javascript+
                  (merge-pathnames #p"main.js"
                                   (app-directory directory
                                                                 :operating-system operating-system))))

(defun insert-package-definition (directory &key operating-system)
  "Insert the package.json into an Electron release."
  (with-open-file (output-stream (merge-pathnames #p"package.json"
                                                  (app-directory directory
                                                                 :operating-system operating-system))
                                 :direction :output
                                 :if-does-not-exist :create)
    (write-string (format nil "{ \"name\": ~S, \"version\": ~S, \"main\": \"main.js\" }"
                          "Ceramic/Electron"
                          (asdf:component-version
                           (asdf:find-system :ceramic)))
                  output-stream)))

(defun prepare-release (directory &key operating-system)
  "Prepare an Electron release."
  (clean-release directory :operating-system operating-system)
  (insert-javascript directory :operating-system operating-system)
  (insert-package-definition directory :operating-system operating-system))

;;; Main

(defvar *electron-version* "0.28.1"
  "The version of Electron to use.")

(defun global-binary-pathname ()
  "The pathname to the downloaded Electron binary. Used for interactive
  testing."
  (binary-pathname (release-directory)
                   :operating-system *operating-system*))

(defun setup ()
  "Set up everything needed to start developing."
  (log-message "Creating Ceramic directories...")
  (ensure-directories-exist *ceramic-directory*)
  (log-message "Downloading a copy of Electron...")
  (ensure-directories-exist (release-directory))
  (progn
    (get-release (release-directory)
                 :operating-system *operating-system*
                 :architecture *architecture*
                 :version *electron-version*)
    (prepare-release (release-directory) :operating-system *operating-system*)))
