(in-package :cl-user)
(defpackage ceramic-test
  (:use :cl :fiveam)
  (:import-from :ceramic.file
                :*ceramic-directory*
                :*buildapp-pathname*
                :wipe-data)
  (:export :run-tests))
(in-package :ceramic-test)

(defun run-tests ()
  (let* ((*ceramic-directory* (asdf:system-relative-pathname :ceramic-test
                                                             #p"t/ceramic/"))
         (*buildapp-pathname* (merge-pathnames #p"buildapp"
                                               *ceramic-directory*))
         (ceramic.log:*logging* t))
    (run! 'ceramic-test.electron.tools:electron-tools)
    (run! 'ceramic-test.electron:tests)
    (run! 'ceramic-test.setup:tests)
    (run! 'ceramic-test.integration:tests)
    (run! 'ceramic-test.misc:tests)
    ;; Cleanup
    (ceramic.file:wipe-data)))
