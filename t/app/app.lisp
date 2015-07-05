(in-package :cl-user)
(defpackage ceramic-test-app
  (:use :cl))
(in-package :ceramic-test-app)

(ceramic.resource:define-resources :ceramic-test-app ()
  (files #p"resources/"))

(ceramic:define-entry-point :ceramic-test-app ()
  (print ceramic.runtime::*releasep*)
  (print (ceramic.resource:resource 'files #p"file.txt"))
  (print
  (let ((window (ceramic:make-window :url "http://google.com/")))
    (ceramic:show-window window)))
