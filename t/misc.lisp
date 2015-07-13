(in-package :cl-user)
(defpackage ceramic-test.misc
  (:use :cl :fiveam)
  (:export :tests))
(in-package :ceramic-test.misc)

(def-suite tests
  :description "Miscellaneous tests.")
(in-suite tests)

(test runtime
  (let ((ceramic.runtime:*releasep* t))
    (is
     (pathnamep (ceramic.runtime:executable-pathname)))
    (is
     (pathnamep (ceramic.runtime:executable-relative-pathname #p"file.txt")))))

(test os
  (is (keywordp (ceramic.os::detect-operating-system)))
  (is (keywordp (ceramic.os::detect-architecture))))
