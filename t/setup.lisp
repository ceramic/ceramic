(in-package :cl-user)
(defpackage ceramic-test.setup
  (:use :cl :fiveam)
  (:export :tests))
(in-package :ceramic-test.setup)

(def-suite tests
  :description "Setup tests.")
(in-suite tests)

(test setup
  (finishes
    (ceramic.setup:setup))
  (is-true
   (probe-file ceramic.file:*buildapp-pathname*))
  (is
   (not
    (uiop:emptyp
     (uiop:directory-files (ceramic.electron:release-directory))))))
