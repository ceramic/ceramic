(in-package :cl-user)
(defpackage ceramic-test.integration
  (:use :cl :fiveam)
  (:export :tests))
(in-package :ceramic-test.integration)

(def-suite tests
  :description "Integration tests.")
(in-suite tests)

(test interactive
  (ceramic:with-interactive ()
    (sleep 1)
    (let ((window (ceramic:make-window :title "My Window")))
      (finishes
       (ceramic:show-window window))
      (finishes
       (ceramic:hide-window window))
      (finishes
       (ceramic:show-window window))
      (finishes
       (ceramic:maximize-window window))
      (finishes
       (ceramic:unmaximize-window window))
      (finishes
       (ceramic:minimize-window window))
      (finishes
       (ceramic:unminimize-window window))
      (finishes
       (ceramic:center-window window))
      (sleep 1)
      (finishes
       (ceramic:close-window window)))))

(defvar *extraction-directory*
  (asdf:system-relative-pathname :ceramic-test-app
                                 #p"extract/"))

(test compiled
  (let* ((app-file (merge-pathnames #p"ceramic-test-app.tar"
                                    *extraction-directory*))
         (binary (merge-pathnames #p"ceramic-test-app"
                                  *extraction-directory*)))
    (ensure-directories-exist *extraction-directory*)
    (finishes
      (ceramic.bundler:bundle :ceramic-test-app
                              :bundle-pathname app-file))
    (is-true
     (probe-file app-file))
    (finishes
      (trivial-extract:extract-tar app-file))
    (is-true
     (probe-file binary))
    (is-true
     (probe-file (merge-pathnames #p"resources/files/file.txt"
                                  *extraction-directory*)))
    (finishes
      (ceramic.util:ensure-executable (merge-pathnames #p"electron/electron"
                                                       *extraction-directory*))
      (ceramic.util:ensure-executable binary))
    (is
     (equal (ceramic.resource:resource-directory 'ceramic-test-app::files)
            (asdf:system-relative-pathname :ceramic-test-app
                                           #p"files/")))
    (is
     (equal (ceramic.resource:resource 'ceramic-test-app::files #p"file.txt")
            (asdf:system-relative-pathname :ceramic-test-app
                                           #p"files/file.txt")))))

(test cleanup
  (uiop:delete-directory-tree *extraction-directory* :validate t))
