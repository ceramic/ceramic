(in-package :cl-user)
(defpackage ceramic-test.driver
  (:use :cl :fiveam)
  (:import-from :ceramic.driver
                :*driver*)
  (:export :driver))
(in-package :ceramic-test.driver)

(def-suite driver
  :description "Driver tests.")
(in-suite driver)

(test lifecycle
  (finishes
    (ceramic.driver:start *driver*))
  (finishes
    (ceramic.driver:stop *driver*)))
