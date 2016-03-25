(in-package :cl-user)
(defpackage ceramic-test.electron
  (:use :cl :fiveam)
  (:import-from :external-program
                :process-output-stream
                :process-status)
  (:import-from :ceramic.electron
                :start-process
                :create-window
                :window-load-url
                :quit)
  (:export :electron-driver))
(in-package :ceramic-test.electron)

(defparameter *process* nil)

(def-suite electron-driver
  :description "Electron driver tests.")
(in-suite electron-driver)

(defvar *test-directory*
  (asdf:system-relative-pathname :ceramic-test #p"t/test/"))

(defun test-download-and-extract (os arch directory)
  (finishes
    (electron-tools:get-release directory
                                :operating-system os
                                :version "0.28.1"
                                :architecture arch))
  (is-true
   (probe-file (electron-tools:binary-pathname directory
                                               :operating-system os))))

(defun test-changes (directory os)
  (finishes
    (ceramic.electron.tools:prepare-release directory :operating-system os))
  (is-true
   (probe-file (merge-pathnames #p"main.js"
                                (electron-tools:app-directory directory :operating-system os))))
  (is-true
   (probe-file (merge-pathnames #p"package.json"
                                (electron-tools:app-directory directory :operating-system os)))))

(defun test-release (os arch)
  (let ((directory (merge-pathnames
                    (make-pathname :directory (list :relative
                                                    (format nil "~A~A" os arch)))
                    *test-directory*)))
    (test-download-and-extract os arch directory)
    (test-changes directory os)))

(test download-electron
  (test-release :linux :64))

(test start-app
  (let ((release (merge-pathnames #p"LINUX64/"
                                  *test-directory*)))
    (finishes
      (setf *process* (start-process release
                                     :operating-system :linux)))))

(test windows
  (finishes
    (create-window *process* "win" nil))
  (finishes
    (window-load-url *process* "win" "https://www.google.com/"))
  (sleep 5))

(test stop-app
  (finishes
    (quit *process*))
  (sleep 1)
  (is
   (equal (process-status *process*)
          :exited)))

(test cleanup
  (finishes
    (uiop:delete-directory-tree *test-directory* :validate t)))
