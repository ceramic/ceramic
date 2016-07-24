(in-package :cl-user)
(defpackage ceramic-test.dialog
  (:use :cl :fiveam)
  (:import-from :ceramic.driver
                :*driver*
                :sync-js)
  (:export :dialog))
(in-package :ceramic-test.dialog)

(def-suite dialog
  :description "Dialog tests.")
(in-suite dialog)

(defclass mock-driver (ceramic.driver::driver)
  ((mock-js :accessor mock-js :initarg :mock-js :initform nil)))

(defmethod sync-js ((driver mock-driver) js)
  (with-slots (mock-js) driver
    (setf mock-js js)))

(test dialog
  (finishes
    (ceramic.driver:start *driver*)
    (change-class *driver* 'mock-driver :mock-js nil))
  (is (string= "return Ceramic.dialog.showErrorBox(\"foo\", \"bar\");"
               (progn (ceramic.dialog:show-error-box "foo" "bar")
                      (mock-js *driver*))))
  (is (string= "return Ceramic.dialog.showMessageBox({\"buttons\":[\"OK\"],\"message\":\"foo\"});"
               (progn (ceramic.dialog:show-message-box "foo")
                      (mock-js *driver*))))
  (is (string= "return Ceramic.dialog.showOpenDialog({\"properties\":[\"openDirectory\"]});"
               (progn (ceramic.dialog:show-open-dialog :open-directory t)
                      (mock-js *driver*))))
  (is (string= "return Ceramic.dialog.showSaveDialog({\"filters\":[{\"name\":\"Cats\",\"extensions\":[\"gif\"]}]});"
               (progn (ceramic.dialog:show-save-dialog :filters '(("Cats" . ("gif"))))
                      (mock-js *driver*))))
  (finishes
    (change-class *driver* 'ceramic.driver::driver)
    (ceramic.driver:stop *driver*)))
