(in-package :cl-user)
(defpackage ceramic-test.integration
  (:use :cl :fiveam)
  (:export :tests))
(in-package :ceramic-test.integration)

(def-suite tests
  :description "Integration tests.")
(in-suite tests)

(test interactive
  (finishes
   (ceramic:interactive))
  (let ((window (ceramic:make-window :title "My Window")))
    (finishes
     (ceramic:show-window window))
    (sleep 1)
    (finishes
     (ceramic:close-window window))))

(test compiled
  (let* ((extraction-directory (asdf:system-relative-pathname :ceramic-test-app
                                                              #p"extract/"))
         (app-file (merge-pathnames #p"ceramic-test-app.zip"
                                    extraction-directory)))
    (ensure-directories-exist extraction-directory)
    (finishes
      (ceramic.bundler:bundle :ceramic-test-app
                              :bundle-pathname app-file))
    (is-true
     (probe-file app-file))
    (finishes
      (trivial-extract:extract-zip app-file))
    (is-true
     (probe-file (merge-pathnames #p"ceramic-test-app"
                                  extraction-directory)))
    (is-true
     (probe-file (merge-pathnames #p"resources/files/file.txt"
                                  extraction-directory)))
    (uiop:delete-directory-tree extraction-directory :validate t)))
