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
         (app-file (merge-pathnames #p"ceramic-test-app.tar"
                                    extraction-directory))
         (binary (merge-pathnames #p"ceramic-test-app"
                                  extraction-directory)))
    (ensure-directories-exist extraction-directory)
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
                                  extraction-directory)))
    (finishes
      (ceramic.util:ensure-executable (merge-pathnames #p"electron/electron"
                                                       extraction-directory))
      (ceramic.util:ensure-executable binary))
    ;; Run the app
    (let* ((process (external-program:start (namestring binary) (list)
                                            :output :stream))
           (stdout (external-program:process-output-stream process)))
      (sleep 0.1)
      (is
       (equal (read-line stdout) "T"))
      (is
       (equal (read-line stdout)
              (namestring
               (merge-pathnames #p"resources/files/file.txt"
                                extraction-directory))))
      (is
       (equal (read-line stdout)
              "opened window")))
    (uiop:delete-directory-tree extraction-directory :validate t)))
