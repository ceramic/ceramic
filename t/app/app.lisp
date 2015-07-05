(in-package :cl-user)
(defpackage ceramic-test-app
  (:use :cl))
(in-package :ceramic-test-app)

(ceramic.resource:define-resources :ceramic-test-app ()
  (files #p"app/resources/"))

(ceramic:define-entry-point :ceramic-test-app ()
  (print
   (uiop:read-file-string
    (ceramic.resource:resource 'files #p"file.txt"))))
