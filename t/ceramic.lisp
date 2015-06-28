(in-package :cl-user)
(defpackage ceramic-test
  (:use :cl :fiveam))
(in-package :ceramic-test)

(run! 'ceramic-test.electron.tools:tests)
