(in-package :cl-user)
(defpackage ceramic.driver
  (:use :cl)
  (:import-from :alexandria
                :if-let)
  (:import-from :ceramic.log
                :*logging*
                :log-message)
  (:import-from :ceramic.runtime
                :*releasep*
                :executable-relative-pathname)
  (:import-from :ceramic.file
                :release-directory)
  (:import-from :ceramic.os
                :*operating-system*)
  (:import-from :electron-tools
                :app-directory
                :binary-pathname)
  (:export :driver
           :*driver*
           :start
           :stop
           :js
           :sync-js)
  (:documentation "The Ceramic driver interface."))
(in-package :ceramic.driver)

;;; Classes

(defclass driver ()
  ((process :accessor driver-process
            :documentation "The Electron process.")
   (context :accessor driver-context
            :type remote-js:buffered-context
            :documentation "The remote-js object.")
   (js-lock :accessor driver-js-lock
            :initform (bt:make-lock "ceramic-js-sync")
            :documentation "A lock object for js sync")
   (js-cond :accessor driver-js-cond
            :initform (bt:make-condition-variable)
            :documentation "A condition variable for js sync")
   (responses :accessor driver-responses
              :initform (make-hash-table :test #'equal)
              :type hash-table
              :documentation "A hash table of message IDs to evaluation results."))
  (:documentation "The Ceramic driver."))

(defvar *driver* (make-instance 'driver)
  "The global driver object.")

;;; Interface

(defgeneric start (driver)
  (:documentation "Start the Electron process and the remote-js server.")

  (:method ((driver driver))
    (log-message "Starting server...")
    (start-remote-js driver)
    (log-message "Starting Electron process...")
    (start-electron driver)
    (log-message "Waiting for startup...")
    (wait-for-client driver)
    (log-message "Electron started")))

(defgeneric stop (driver)
  (:documentation "Stop the Electron process and the remote-js server.")

  (:method ((driver driver))
    (log-message "Stopping Electron process...")
    (stop-electron driver)
    (log-message "Stopping server...")
    (stop-remote-js driver)))

(defgeneric js (driver js)
  (:documentation "Evaluate a string of JavaScript in the Electron process.")

  (:method ((driver driver) js)
    (declare (type string js))
    (remote-js:eval (driver-context driver) js)))

(defgeneric sync-js (driver js)
  (:documentation "Synchronously evaluate JavaScript in the Electron process,
  returning a string containing the result of the evaluation.")

  (:method ((driver driver) js)
    (let* ((message-id (uuid:format-as-urn nil (uuid:make-v4-uuid)))
           (full-js (format nil "Ceramic.syncEval(~S, (function() { ~A }))"
                            message-id
                            js)))
      ;; Send the message
      (js driver full-js)
      ;; And wait for the reply
      (with-slots (responses js-lock js-cond) driver
        (bt:with-lock-held (js-lock)
          (loop
            (multiple-value-bind (response found)
                (gethash message-id responses)
              (if found
                  ;; We got a reply
                  (progn
                    (remhash message-id responses)
                    (return-from sync-js response))
                  ;; Not yet
                  (bt:condition-wait js-cond js-lock)))))))))

(defgeneric port (driver)
  (:documentation "Return the port the WebSockets server is running on.")

  (:method ((driver driver))
    (remote-js:context-port (driver-context driver))))

(defgeneric address (driver)
  (:documentation "Return the address the WebSockets server is running on.")

  (:method ((driver driver))
    (remote-js:context-address (driver-context driver))))

;;; IPC

(defgeneric on-message (driver message)
  (:documentation "Receive a message from a WebSockets client.")

  (:method ((driver driver) message)
    (declare (type string message))
    (let ((data (cl-json:decode-json-from-string message)))
      (with-slots (responses js-lock js-cond) driver
        (bt:with-lock-held (js-lock)
          (setf (gethash (rest (assoc :id data :test #'eq)) responses)
                (rest (assoc :result data :test #'eq)))
          (bt:condition-notify js-cond))))))

;;; Internals

(defmethod start-electron ((driver driver))
  "Start the Electron process."
  (let ((directory (if *releasep*
                       (executable-relative-pathname #p"electron/")
                       (release-directory))))
    (with-slots (process) driver
      (setf process
            (external-program:start (binary-pathname directory
                                                     :operating-system ceramic.os:*operating-system*)
                                    (list (app-directory directory
                                                         :operating-system ceramic.os:*operating-system*)
                                          (address driver)
                                          (write-to-string (port driver)))
                                    :output (when *logging* *standard-output*)
                                    :error :output))))
  (values))

(defmethod stop-electron ((driver driver))
  "Stop the Electron process."
  (with-slots (process) driver
    (handler-case
        (js driver "Ceramic.quit()")
      (t ()
        (warn "Error quitting the Electron process. Forcing shutdown...")
        (external-program:signal-process process :killed))))
  (values))

(defmethod start-remote-js ((driver driver))
  "Start the remote-js server."
  (with-slots (context) driver
    (setf context
          (remote-js:make-buffered-context :address "localhost"
                                           :callback
                                           #'(lambda (message)
                                               (on-message driver message))))
    (remote-js:start context))
  (values))

(defmethod wait-for-client ((driver driver))
  "Wait for the client to connect to the WebSockets server."
  (loop until (remote-js:context-connected-p (driver-context driver))))

(defmethod stop-remote-js ((driver driver))
  "Stop the remote-js server."
  (with-slots (context) driver
    (remote-js:stop context))
  (slot-makunbound driver 'context)
  (values))
