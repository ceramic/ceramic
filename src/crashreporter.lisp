(in-package :cl-user)
(defpackage ceramic.crashreporter
  (:use :cl)
  (:import-from :ceramic.driver
                :*driver* :js)
  (:export :start-crash-reporter)
  (:documentation "Start Electron's Crash Reporter."))
(in-package :ceramic.crashreporter)

(defun start-crash-reporter (company-name submit-url &key options)
  (flet ((remove-null-values (plist)
           (loop for (key value) on plist by #'cddr
              if value
              appending (list key value))))
    (let ((opt (cl-json:encode-json-plist-to-string
                (remove-null-values
                 (concatenate 'list
                              (list "companyName" company-name
                                    "submitURL" submit-url)
                              options)))))
      (js *driver*
          (format nil "Ceramic.startCrashReporter(~A)" opt)))))
