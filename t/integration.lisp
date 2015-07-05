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
  (let* ((app-directory (asdf:system-relative-pathname :ceramic #p"t/app/"))
         (app-file (merge-pathnames #p"ceramic-test-app.zip"
                                    app-directory)))
    (finishes
      (ceramic.bundler:bundle :ceramic-test-app))
    (is-true
     (probe-file app-file))
    (finishes
      (trivial-extract:extract-zip app-file))
    (is-true
     (probe-file (merge-pathnames #p"ceramic-test-app"
                                  app-directory)))
    (when (probe-file app-file)
      (delete-file app-file))))
