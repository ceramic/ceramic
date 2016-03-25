(in-package :cl-user)
(defpackage ceramic.driver
  (:use :cl)
  (:import-from :alexandria
                :if-let)
  (:import-from :ceramic.log
                :log-message)
  (:import-from :ceramic.runtime
                :*releasep*
                :executable-relative-pathname)
  (:import-from :ceramic.file
                :release-directory)
  (:import-from :ceramic.os
                :*operating-system*)
  (:import-from :electron-tools
                :binary-pathname)
  (:export :driver
           :*driver*
           :start
           :stop
           :js)
  (:documentation "The Ceramic driver interface."))
(in-package :ceramic.driver)

;;; Classes

(defclass driver ()
  ((process :accessor driver-process
            :documentation "The Electron process.")
   (context :accessor driver-context
            :type remote-js:buffered-context
            :documentation "The remote-js object.")
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
    (log-message "Starting Electron process...")
    (start-electron driver)
    (log-message "Starting server...")
    (start-remote-js driver)))

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
    (let ((message-id (uuid:format-as-urn nil (uuid:make-v4-uuid))))
      ;; Send the message
      (js driver (format nil "Ceramic.syncEval(~S, (function() { ~A }))"
                         message-id
                         js))
      ;; And wait for the reply
      (with-slots (responses) driver
        (loop
           (if-let (response (gethash message-id responses))
             ;; We got a reply
             (return-from sync-js response))
           ;; Sleep a millisecond
           (sleep 0.001))))))

(defgeneric port (driver)
  (:documentation "Return the port the WebSockets server is running on.")

  (:method ((driver driver))
    (remote-js:context-port (driver-context driver))))

;;; IPC

(defgeneric on-message (driver message)
  (:documentation "Receive a message from a WebSockets client.")

  (:method ((driver driver) message)
    (declare (type string message))
    (let ((data (cl-json:decode-json-from-string message)))
      (with-slots (responses) driver
        (setf (gethash (gethash "id" data) responses)
              (gethash "result" data))))))

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
                                    (list (write-to-string (port driver)))))))
  (values))

(defmethod stop-electron ((driver driver))
  "Stop the Electron process."
  (with-slots (process) driver
    (handler-case
        ;;(ceramic.electron:quit process) FIXME: implement this
        (error "fixme")
      (t ()
        (warn "Error quitting the Electron process. Forcing shutdown...")
        (external-program:signal-process process :killed))))
  (values))

(defmethod start-remote-js ((driver driver))
  "Start the remote-js server."
  (with-slots (context) driver
    (setf context
          (remote-js:make-buffered-context :callback
                                           #'(lambda (message)
                                               (on-message driver message))))
    (remote-js:start context))
  (values))

(defmethod stop-remote-js ((driver driver))
  "Stop the remote-js server."
  (with-slots (context) driver
    (remote-js:stop context))
  (slot-makunbound driver 'context)
  (values))
