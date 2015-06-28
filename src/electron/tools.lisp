(in-package :cl-user)
(defpackage ceramic.electron.tools
  (:use :cl)
  (:export :+download-url+
           :download-url
           :download
           :extract
           :binary-pathname
           :clean-release
           :insert-javascript
           :insert-package:definition)
  (:documentation "Tools for Electron."))
(in-package :ceramic.electron.tools)

;;; Constants

(defparameter +download-url+
  "https://github.com/atom/electron/releases/download/v~A/electron-v~A-~A-~A.zip"
  "A format string for the download URL. Values are: version, version, operating
system, architecture.")

(defparameter +main-javascript+
  (asdf:system-relative-pathname :ceramic-electron #p"src/main.js")
  "Pathname to the JavaScript file for the main process.")

;;; OS-specific utilities

(defun download-url (&key operating-system version architecture)
  "A download link to the specified release."
  (let ((os (case operating-system
              (:linux "linux")
              (:mac "darwin")
              (:windows "win32")))
        (arch (case architecture
                (:64 "x64")
                (:32 "ia32"))))
  (format nil +download-url+ version version os arch)))

(defun download (pathname &key operating-system version architecture)
  "Download a release to a pathname."
  (trivial-download:download (download-url :operating-system operating-system
                                           :version version
                                           :architecture architecture)
                             pathname))

(defun extract (pathname)
  "Extract an Electron snapshot into its containing directory."
  (trivial-extract:extract-zip pathname)
  ;; When on Unix, set the executable bit on the file
  #-(or win32 mswindows)
  (let* ((parent (uiop:pathname-directory-pathname pathname))
         (binary (or (probe-file (binary-pathname parent
                                                  :operating-system :linux))
                     (probe-file (binary-pathname parent
                                                  :operating-system :mac)))))
    (when binary
      (setf (osicat:file-permissions binary)
            (list :user-read
                  :user-write
                  :user-exec)))))

(defun binary-pathname (directory &key operating-system)
  "The pathname to the Electron binary inside the directory it was extracted to."
  (merge-pathnames (case operating-system
                     (:linux #p"electron")
                     (:mac #p"Electron.app/Contents/MacOS/Electron")
                     (:windows #p"electron.exe"))
                   directory))

(defun clean-release (directory)
  "Clean up default files from an Electron release."
  (let ((app-files (list #p"main.js"
                         #p"default_app.js"
                         #p"index.html"
                         #p"package.json")))
    (loop for file in app-files do
      (let ((pathname (merge-pathnames file
                                       (merge-pathnames #p"resources/default_app/"
                                                        directory))))
        (delete-file pathname)))))

(defun insert-javascript (directory)
  "Insert the main process JavaScript into an Electron release."
  (uiop:copy-file +main-javascript+
                  (merge-pathnames #p"resources/default_app/main.js"
                                   directory)))

(defun insert-package-definition (directory &key name version)
  "Insert the package.json into an Electron release."
  (with-open-file (output-stream (merge-pathnames #p"resources/default_app/package.json"
                                                  directory)
                                 :direction :output
                                 :if-does-not-exist :create)
    (write-string (jonathan:to-json (list (cons "name" name)
                                          (cons "version" version)
                                          (cons "main" "main.js"))
                                    :from :alist)
                  output-stream)))
