(in-package :cl-user)
(defpackage ceramic-test.electron.tools
  (:use :cl :fiveam)
  (:import-from :ceramic.electron.tools
                :get-release
                :binary-pathname
                :app-directory
                :prepare-release)
  (:export :*test-directory*
           :tests))
(in-package :ceramic-test.electron.tools)

(defvar *test-directory*
  (asdf:system-relative-pathname :ceramic-test #p"t/test/"))

(defun test-download-and-extract (os arch directory)
  (finishes
   (get-release directory
                :operating-system os
                :version "0.28.1"
                :architecture arch))
  (is-true
   (probe-file (binary-pathname directory
                                :operating-system os))))

(defun test-changes (directory os)
  (finishes
    (prepare-release directory :operating-system os))
  (is-true
   (probe-file (merge-pathnames #p"main.js"
                                (app-directory directory :operating-system os))))
  (is-true
   (probe-file (merge-pathnames #p"package.json"
                                (app-directory directory :operating-system os)))))

(defun test-release (os arch)
  (let ((directory (merge-pathnames
                    (make-pathname :directory (list :relative
                                                    (format nil "~A~A" os arch)))
                    *test-directory*)))
    (test-download-and-extract os arch directory)
    (test-changes directory os)))

(def-suite tests
  :description "Electron tools tests.")
(in-suite tests)

(test linux64-release
 (test-release :linux :64))

(test linux32-release
 (test-release :linux :32))

(test windows-download
 (test-release :windows :64))

(test mac-download
 (test-release :mac :64))
