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
                :insert-package-json)
  (:export :tests))
(in-package :ceramic-test.electron.tools)

;; Utilities

(defvar *test-directory*
  (asdf:system-relative-pathname :ceramic-test #p"t/test/"))

(defun test-download-and-extract (os arch)
  (when (probe-file *test-directory*)
    (uiop:delete-directory-tree *test-directory* :validate t))
  (let ((pathname (merge-pathnames #p"electron.zip"
                                   *test-directory*)))
    (finishes
      (download pathname
                :operating-system os
                :version "0.28.1"
                :architecture arch))
    (finishes
      (extract pathname))
    (is-true
     (probe-file (binary-pathname *test-directory*
                                  :operating-system os)))
    ;; Changes
    (finishes
      (clean-release *test-directory*))
    (finishes
      (insert-javascript *test-directory*))
    (is-true
     (probe-file (merge-pathnames #p"main.js"
                                  (app-directory *test-directory*))))))

;;; Tests

(def-suite tests
  :description "Electron tools tests.")
(in-suite tests)

(test linux-download
  (test-download-and-extract :linux :64)
  #|(test-download-and-extract :linux :32)|#)

#| Takes too long
(test windows-download
  (test-download-and-extract :windows :64))

(test mac-download
  (test-download-and-extract :mac :64))
|#
