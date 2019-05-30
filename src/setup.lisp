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
  (:export :setup)
  (:documentation "Set up everything needed to develop with Ceramic."))
(in-package :ceramic.setup)

;;; Dealing with Electron releases

(defparameter +main-javascript+
  (asdf:system-relative-pathname :ceramic #p"src/main.js")
  "Pathname to the JavaScript file for the main process.")

(defparameter +ws-module+
  (asdf:system-relative-pathname :ceramic #p"node_modules/ws/"))

(defun clean-release (directory &key operating-system)
  "Clean up default files from an Electron release."
  (let ((app-files (list #p"main.js"
                         #p"default_app.js"
                         #p"index.html"
                         #p"package.json")))
    (loop for file in app-files do
      (let ((pathname (merge-pathnames file
                                       (app-directory directory :operating-system operating-system))))
        (when (probe-file pathname)
          (delete-file pathname))))))

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

(defun copy-ws-module (directory &key operating-system)
  "Copy the WebSockets module."
  (copy-directory:copy +ws-module+
                       (merge-pathnames #p"node_modules/ws/"
                                        (app-directory directory
                                                       :operating-system operating-system))))

(defun prepare-release (directory &key operating-system)
  "Prepare an Electron release."
  (ensure-directories-exist (app-directory directory :operating-system operating-system))
  (clean-release directory :operating-system operating-system)
  (insert-javascript directory :operating-system operating-system)
  (insert-package-definition directory :operating-system operating-system)
  (copy-ws-module directory :operating-system operating-system))

;;; Main

(defparameter *electron-version* "5.0.2"
  "The version of Electron to use.")

(defun setup (&key force)
  "Set up everything needed to start developing."
  (log-message "Creating Ceramic directories...")
  (ensure-directories-exist (release-directory))
  (if (or (uiop:emptyp (uiop:directory-files (release-directory)))
          force)
      (progn
        (log-message "Downloading a copy of Electron...")
        (ensure-directories-exist (release-directory))
        (get-release (release-directory)
                     :operating-system *operating-system*
                     :architecture *architecture*
                     :version *electron-version*))
      (log-message "Already downloaded. Use :force t to force download."))
  (log-message "Preparing the files...")
  (prepare-release (release-directory) :operating-system *operating-system*)
  (values))
