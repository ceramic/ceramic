(in-package :cl-user)
(defpackage ceramic.browser
  (:use :cl)
  (:import-from :ceramic.os
                :linux
                :windows
                :mac)
  (:export :run-browser-command)
  (:documentation "Run Chromium instances."))
(in-package :ceramic.browser)
