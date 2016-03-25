(in-package :cl-user)
(defpackage ceramic.setup
  (:use :cl)
  (:documentation "Set up everything needed to develop with Ceramic."))
(in-package :ceramic.setup)

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
    (write-string (jonathan:to-json (list (cons "name" "Ceramic/Electron")
                                          (cons "version"
                                                (asdf:component-version
                                                 (asdf:find-system :ceramic)))
                                          (cons "main" "main.js"))
                                    :from :alist)
                  output-stream)))

(defun prepare-release (directory &key operating-system)
  "Prepare an Electron release."
  (clean-release directory :operating-system operating-system)
  (insert-javascript directory :operating-system operating-system)
  (insert-package-definition directory :operating-system operating-system))
