(in-package :cl-user)
(defpackage ceramic
  (:use :cl)
  (:import-from :ceramic.driver
                :*driver*)
  (:import-from :ceramic.setup
                :setup)
  (:import-from :ceramic.runtime
                :*releasep*)
  (:export :setup
           :start
           :stop
           :define-entry-point)
  (:documentation "The main interface."))
(in-package :ceramic)

;;; Lifecycle

(defun start ()
  "Start the Electron process."
  (ceramic.driver:start *driver*))

(defun stop ()
  "Stop the Electron process."
  (ceramic.driver:stop *driver*))

(defun quit (&optional (exit-status 0))
  "Kill the Electron process and the Lisp process."
  (stop)
  (uiop:quit exit-status))

;;; Entry point for released applications

(defpackage ceramic-entry
  (:use :cl))

(defmacro define-entry-point (system-name () &body body)
  "Define the application's entry point."
  (let ((entry-point (intern (symbol-name system-name)
                             (find-package :ceramic-entry)))
        (arguments (gensym)))
    `(defun ,entry-point (,arguments)
       (declare (ignore ,arguments))
       ;; Start the executable-relative Electron process
       (let ((*releasep* t))
         (start)
         (handler-case
             (progn
               ,@body
               (quit))
           (t () (quit)))))))
