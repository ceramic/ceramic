(in-package :cl-user)
(defpackage ceramic-test.misc
  (:use :cl :fiveam)
  (:export :misc))
(in-package :ceramic-test.misc)

(def-suite misc
  :description "Miscellaneous tests.")
(in-suite misc)

(test runtime
  (let ((ceramic.runtime:*releasep* t))
    (is
     (pathnamep (ceramic.runtime:executable-pathname)))
    (is
     (pathnamep (ceramic.runtime:executable-relative-pathname #p"file.txt"))))
  (let ((ceramic.runtime:*releasep* nil))
    (signals ceramic.error:not-in-release
     (ceramic.runtime:executable-pathname))))

(test os
  (is (keywordp (ceramic.os::detect-operating-system)))
  (is (keywordp (ceramic.os::detect-architecture))))
