(in-package :cl-user)
(defpackage ceramic.download
  (:use :cl)
  (:export :*os-type*
           :*chromium-snapshot*
           :download-url
           :download-snapshot
           :extract-snapshot)
  (:documentation "Download a snapshot of Chromium to run the app."))
(in-package :ceramic.download)

(defparameter *os-type*
  (cond
    ((uiop:os-windows-p)
     :windows)
    ((uiop:os-macosx-p)
     :mac)
    ((eq (uiop:operating-system) :linux)
     (if (eq (uiop:architecture) :x64)
         :linux64
         :linux32))
    (t
     (error 'ceramic.error:unsupported-os
            :os-name (uiop:operating-system))))
  "A keyword describing the type of the OS: :linux64, :linux32, :windows
and :mac.")

(defparameter +chromium-download-url-format+
  "https://commondatastorage.googleapis.com/chromium-browser-snapshots/index.html?prefix=~A/~A/")

(defparameter +chromium-download-prefix-map+
  (list :linux64 "Linux_x64"
        :linux32 "Linux"
        :windows "Win"
        :mac "Mac")
  "A plist that maps the OS type to the OS name used in the download URL.")

(defparameter +chromium-archive-directory+
  (list :linux64 #p"chrome-linux/"
        :linux32 #p"chrome-linux/"
        :windows #p"chrome-windows32/"
        :mac #p"chrome-mac/")
  "A plist that maps the OS type to the name of the folder inside the Chromium archive.")

(defparameter +chromium-binary-pathname+
  (list :linux64 #p"chrome"
        :linux32 #p"chrome"
        :windows #p"chrome.exe"
        :mac #p"Chromium.app")
  "A plist that maps the OS type to the pathname of the Chromium executable.")

(defun chromium-download-url (os-type snapshot-number)
  "Return the download URL for a given OS type and snapshot number."
  (format nil
          +chromium-download-url-format+
          (getf +chromium-download-prefix-map+ os-type)
          snapshot-number))

(defparameter *chromium-snapshot*
  321210
  "The ID of the Chromium snapshot that will be downloaded.")

(defun download-url ()
  "Return the URL to the Chromium download for this operating system and
architecture."
  (chromium-download-url *os-type* *chromium-snapshot*))

(defun snapshot-pathname (directory)
  (make-pathname :name "chrome"
                 :type "zip"
                 :defaults directory))

(defun download-snapshot (directory)
  "Download the Chromium snapshot for this operating system to the directory."
  (trivial-download:download (download-url) (snapshot-pathname directory)))

(defun extract-snapshot (directory)
  "Unzip a Chromium snapshot in the directory where it was downloaded."
  (trivial-extract:extract-zip (snapshot-pathname directory)))

(defun binary-pathname (directory)
  "Return the pathnme to the binary in the directory where the snapshot was extracted."
  (let ((extracted-directory (getf +chromium-archive-directory+ *os-type*))
        (binary-pathname (getf +chromium-binary-pathname+ *os-type*)))
    (merge-pathnames binary-pathname
                     (merge-pathnames extracted-directory
                                      directory))))
