(in-package :cl-user)
(defpackage ceramic.driver
  (:use :cl)
  (:import-from :ceramic.log
                :log-message)
  (:import-from :ceramic.runtime
                :*releasep*)
  (:documentation "The Ceramic driver interface."))
(in-package :ceramic.driver)

;;; Classes

(defclass driver ()
  ((process :accessor driver-process
            :documentation "The Electron process.")
   (context :accessor driver-context
            :type remote-js:buffered-context
            :documentation "The remote-js object."))
  (:documentation "The Ceramic driver."))

(defvar *driver* (make-instance 'driver)
  "The global driver object.")

;;; Interface

(defgeneric start (driver)
  (:documentation "Start the Electron process and the remote-js server.")

  (:method ((driver driver))
    (if *releasep*
        (start-release-electron driver)
        (start-local-electron driver))
    (log-message "Starting server...")
    (start-remote-js driver)))

(defgeneric stop (driver)
  (:documentation "Stop the Electron process and the remote-js server.")

  (:method ((driver driver))
    (log-message "Stopping process...")
    (stop-process driver)
    (log-message "Stopping server...")
    (stop-remote-js driver)))

(defgeneric on-message (driver message)
  (:documentation "React to a message from a WebSockets client."))

(defgeneric port (driver)
  (:documentation "Return the port the WebSockets server is running on.")

  (:method ((driver driver))
    (remote-js:context-port (driver-context driver))))

;;; Internals

(defmethod start-release-electron ((driver driver))
  "Start the Electron process in the release directory."
  (log-message "Starting Electron process...")
  (with-slots (process) driver
    (setf process
          (ceramic.electron:start-process (executable-relative-pathname #p"electron/")
                                          :operating-system ceramic.os:*operating-system*)))
  (values))

(defmethod start-local-electron ((driver driver))
  "Start the Electron process from the Ceramic directory for interactive use."
  (log-message "Starting Electron process for interactive use...")
  (with-slots (process) driver
    (setf process
          (ceramic.electron:start-process (release-directory)
                                          :operating-system ceramic.os:*operating-system*)))
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

(defmethod stop-process ((driver driver))
  "Stop the Electron process."
  (with-slots (process) driver
    (handler-case
        (ceramic.electron:quit process)
      (t ()
        (warn "Error quitting the Electron process. Forcing shutdown...")
        (external-program:signal-process *process* :killed))))
  (values))

(defmethod stop-remote-js ((driver driver))
  "Stop the remote-js server."
  (with-slots (context) driver
    (remote-js:stop context))
  (slot-makunbound driver 'context)
  (values))
