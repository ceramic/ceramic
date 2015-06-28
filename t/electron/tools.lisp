(in-package :cl-user)
(defpackage ceramic-test.electron.tools
  (:use :cl :fiveam)
  (:import-from :ceramic.electron.tools
                :download
                :extract
                :binary-pathname
                :app-directory
                :clean-release
                :insert-javascript
                :insert-package-definition)
  (:export :tests))
(in-package :ceramic-test.electron.tools)

;; Utilities

(defvar *test-directory*
  (asdf:system-relative-pathname :ceramic-test #p"t/test/"))

(defun test-download-and-extract (os arch directory)
  (let ((pathname (merge-pathnames #p"electron.zip"
                                   directory)))
    (unless (probe-file pathname)
      (finishes
        (download pathname
                  :operating-system os
                  :version "0.28.1"
                  :architecture arch)))
    (finishes
      (extract pathname))
    (is-true
     (probe-file (binary-pathname directory
                                  :operating-system os)))))

(defun test-changes (directory)
  (finishes
    (clean-release directory))
  (finishes
    (insert-javascript directory))
  (is-true
   (probe-file (merge-pathnames #p"main.js"
                                (app-directory directory))))
  (finishes
    (insert-package-definition directory
                               :name "test"
                               :version "0.1"))
  (is-true
   (probe-file (merge-pathnames #p"package.json"
                                (app-directory directory)))))

(defun test-release (os arch)
  (let ((directory (merge-pathnames
                    (make-pathname :directory (list :relative
                                                    (format nil "~A~A" os arch)))
                    *test-directory*)))
    (test-download-and-extract os arch directory)
    (test-changes directory)))

;;; Tests

(def-suite tests
  :description "Electron tools tests.")
(in-suite tests)

(test linux64-release
 (test-release :linux :64))

#| Takes too long
(test linux32-release
 (test-release :linux :32))

(test windows-download
 (test-release :windows :64))

(test mac-download
 (test-release :mac :64))
|#
