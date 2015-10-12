(in-package :cl-user)
(defpackage ceramic-test.setup
  (:use :cl :fiveam)
  (:export :setup))
(in-package :ceramic-test.setup)

(def-suite setup
  :description "Setup tests.")
(in-suite setup)

(test setup
  (finishes
    (ceramic.setup:setup))
  (is-true
   (probe-file ceramic.file:*buildapp-pathname*))
  (is
   (not
    (uiop:emptyp
     (uiop:directory-files (ceramic.electron:release-directory))))))
