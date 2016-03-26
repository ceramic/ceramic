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
    (ceramic.driver:js *driver* "console.log('hello!')"))
  (is
   (= (ceramic.driver:sync-js *driver* "return 1+1")
      2))
  (finishes
    (ceramic.driver:stop *driver*)))
