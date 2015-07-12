(in-package :cl-user)
(defpackage ceramic-test
  (:use :cl :fiveam)
  (:import-from :ceramic.file
                :*ceramic-directory*
                :*buildapp-pathname*
                :wipe-data))
(in-package :ceramic-test)

(let* ((*ceramic-directory* (asdf:system-relative-pathname :ceramic-test
                                                           #p"t/ceramic/"))
       (*buildapp-pathname* (merge-pathnames #p"buildapp"
                                             *ceramic-directory*)))
  (run! 'ceramic-test.electron.tools:tests)
  (run! 'ceramic-test.electron:tests)
  (print *ceramic-directory*)
  (print (ceramic.electron:release-directory))
  (run! 'ceramic-test.setup:tests)
  (run! 'ceramic-test.integration:tests)
  ;; Cleanup
  (uiop:delete-directory-tree *ceramic-directory* :validate t))
