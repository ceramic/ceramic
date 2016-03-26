(in-package :cl-user)
(defpackage ceramic-test.resource
  (:use :cl :fiveam)
  (:export :resource))
(in-package :ceramic-test.resource)

(def-suite resource
  :description "Resource tests.")
(in-suite resource)

(test resources
  (signals ceramic.error:no-such-tag
    (ceramic.resource::get-resource 'non-existent-tag))
  (finishes
    (ceramic.resource:define-resources :ceramic-test ()
      (tag #p"dir/")))
  (finishes
    (ceramic.resource::get-resource 'tag))
  (finishes
   (ceramic.resource:resource-directory 'tag))
  (let ((ceramic.runtime:*releasep* t))
    (finishes
      (ceramic.resource:resource-directory 'tag)))
  (finishes
    (setf ceramic.resource::*resources* (list))))
