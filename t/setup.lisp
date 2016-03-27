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
  (finishes
    (ceramic.setup:setup))
  (is
   (not
    (uiop:emptyp
     (uiop:directory-files (ceramic.file:release-directory))))))
