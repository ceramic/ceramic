(in-package :cl-user)
(defpackage electron-tools
  (:use :cl)
  (:export :+download-url+
           :download-url
           :download
           :binary-pathname)
  (:documentation "Tools for Electron."))
(in-package :electron-tools)

;;; Constants

(defparameter +download-url+
  "https://github.com/atom/electron/releases/download/v~A/electron-v~A-~A-~A.zip"
  "A format string for the download URL. Values are: version, version, operating
system, architecture.")

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

(defun binary-pathname (directory &key operating-system)
  "The pathname to the Electron binary inside the directory it was extracted to."
  (merge-pathnames (case operating-system
                     (:linux #p"electron")
                     (:mac #p"Electron.app/Contents/MacOS/Electron")
                     (:windows #p"electron.exe"))
                   directory))
