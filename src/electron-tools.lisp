(in-package :cl-user)
(defpackage electron-tools
  (:use :cl)
  (:export :+download-url+
           :download-url)
  (:documentation "Tools for Electron."))
(in-package :electron-tools)

;;; Constants

(defparameter +download-url+
  "https://github.com/atom/electron/releases/download/v~A/electron-v~A-~A-~A.zip"
  "A format string for the download URL. Values are: version, version, operating
system, architecture.")

;;; OS-specific utilities

(defun download-url (&key operating-system version architecture)
  "A download link to the snapshot, for a given operating system, version and
architecture."
  (let ((os (case operating-system
              (:linux "linux")
              (:mac "darwin")
              (:windows "win32")))
        (arch (case architecture
                (:64 "x64")
                (:32 "ia32"))))
  (format nil +download-url+ os os version arch)))
