(in-package :cl-user)
(defpackage ceramic-test-app
  (:use :cl))
(in-package :ceramic-test-app)

(ceramic.resource:define-resources :ceramic-test-app ()
  (files #p"files/"))

(ceramic:define-entry-point :ceramic-test-app ()
  (format t "~A~%~A~%"
          ceramic.runtime::*releasep*
          (ceramic.resource:resource 'files #p"file.txt"))
  (let ((window (ceramic:make-window :url "http://google.com/")))
    (ceramic:show window)
    (sleep 1)
    (ceramic:quit)))
