(in-package :cl-user)
(defpackage ceramic-test.crashreporter
  (:use :cl :fiveam)
  (:import-from :ceramic.driver
                :*driver*)
  (:export :crashreporter))
(in-package :ceramic-test.crashreporter)

(def-suite crashreporter
    :description "Crashreporter tests.")
(in-suite crashreporter)

(test crashreporter
  (finishes
    (ceramic.driver:start *driver*))
  (finishes
    (ceramic.crashreporter:start-crash-reporter "FOO" "127.0.0.1"))
  (finishes
    (ceramic.driver:stop *driver*)))
