(in-package :cl-user)
(defpackage ceramic.electron.tools
  (:use :cl)
  (:export :+download-url+
           :download-url
           :get-release
           :binary-pathname
           :app-directory
           :prepare-release)
  (:documentation "Tools for Electron."))
(in-package :ceramic.electron.tools)

;;; Constants

(defparameter +download-url+
  "https://github.com/atom/electron/releases/download/v~A/electron-v~A-~A-~A.zip"
  "A format string for the download URL. Values are: version, version, operating
system, architecture.")

(defparameter +main-javascript+
  (asdf:system-relative-pathname :ceramic #p"src/electron/main.js")
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
  (if (probe-file #P"/usr/bin/unzip")
      ;; Electron archive file for Darwin has symbolic links inside,
      ;; and zip:unzip doesn't handle them.
      ;; for now avoid problem from this by using /usr/bin/unzip
      (external-program:run #P"/usr/bin/unzip"
			      (list pathname
				    "-d"
				    (cl-fad:pathname-directory-pathname pathname)))
      (trivial-extract:extract-zip pathname))
  ;; When on Unix, set the executable bit on the file
  #-(or win32 mswindows)
  (let* ((parent (uiop:pathname-directory-pathname pathname))
         (binary (or (probe-file (binary-pathname parent
                                                  :operating-system :linux))
                     (probe-file (binary-pathname parent
                                                  :operating-system :mac)))))
    (when binary
      (ceramic.util:ensure-executable binary))))

(defun get-release (directory &key operating-system version architecture)
  "Download an Electron release to the directory."
  (let ((archive (merge-pathnames #p"electron.zip" directory)))
    ;; Download the release to the directory
    (download archive :operating-system operating-system
                      :version version
                      :architecture architecture)
    ;; Extract it
    (extract archive)
    ;; Delete it
    (delete-file archive)
    t))

(defun binary-pathname (directory &key operating-system)
  "The pathname to the Electron binary inside the directory it was extracted to."
  (merge-pathnames (case operating-system
                     (:linux #p"electron")
                     (:mac #p"Electron.app/Contents/MacOS/Electron")
                     (:windows #p"electron.exe")
                     (t (error "Unsupported operating system.")))
                   directory))

(defun app-directory (directory &key operating-system)
  "The pathname to the application directory of an Electron release."
  (merge-pathnames (if (eq operating-system :mac)
                       #p"Electron.app/Contents/Resources/default_app/"
                       #p"resources/default_app/")
                   directory))

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
