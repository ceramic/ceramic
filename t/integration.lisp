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
