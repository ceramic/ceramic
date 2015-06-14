(in-package :cl-user)
(defpackage electron-tools-test
  (:use :cl :fiveam))
(in-package :electron-tools-test)

;; Utilities

(defvar *test-directory*
  (asdf:system-relative-pathname :electron-tools-test #p"t/test/"))

(defun test-download-and-extract (os arch)
  (when (probe-file *test-directory*)
    (uiop:delete-directory-tree *test-directory* :validate t))
  (let ((pathname (merge-pathnames #p"electron.zip"
                                   *test-directory*)))
    (finishes
      (electron-tools:download pathname
                               :operating-system os
                               :version "0.28.1"
                               :architecture arch))
    (finishes
      (trivial-extract:extract-zip pathname))
    (is-true
     (probe-file (electron-tools:binary-pathname *test-directory*
                                                 :operating-system os)))))

;;; Tests

(def-suite tests
  :description "electron-tools tests.")
(in-suite tests)

(test linux-download
  (test-download-and-extract :linux :64)
  (test-download-and-extract :linux :32))

(test windows-download
  (test-download-and-extract :windows :64))

(test mac-download
  (test-download-and-extract :mac :64))

(run! 'tests)
