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
    (is
     (stringp (ceramic.window:title win)))
    (is
     (stringp (ceramic.window:url win)))
    (finishes
      (ceramic.window:stop-loading win))
    (finishes
      (ceramic.window:back win))
    (finishes
      (ceramic.window:forward win))
    (finishes
      (ceramic.window:undo win))
    (finishes
      (ceramic.window:cut win))
    (finishes
      (ceramic.window:copy win))
    (finishes
      (ceramic.window:paste win))
    (finishes
      (ceramic.window:select-all win))
    (finishes
      (ceramic.window:unselect win))
    (finishes
      (ceramic.window:open-dev-tools win))
    (finishes
      (ceramic.window:close-dev-tools win))
    (sleep 1)
    (finishes
      (ceramic.window:close win)))
  (finishes
    (ceramic.driver:stop *driver*)))
