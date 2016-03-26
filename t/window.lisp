(in-package :cl-user)
(defpackage ceramic-test.window
  (:use :cl :fiveam)
  (:import-from :ceramic.driver
                :*driver*)
  (:export :window))
(in-package :ceramic-test.window)

(def-suite window
  :description "Window tests.")
(in-suite window)

(test window
  (finishes
    (ceramic.driver:start *driver*))
  (let ((win (ceramic.window:make-window :url "http://www.google.com/")))
    (finishes
      (ceramic.window:show win))
    (sleep 1)
    (finishes
      (ceramic.window:hide win))
    (sleep 1)
    (finishes
      (ceramic.window:show win))
    (sleep 1)
    (finishes
      (ceramic.window:close win)))
  (finishes
    (ceramic.driver:stop *driver*)))
