(in-package :cl-user)
(defpackage ceramic
  (:use :cl)
  (:documentation "The main interface."))
(in-package :ceramic)


;;; Entry point for released applications

(defpackage ceramic-entry
  (:use :cl))

(defmacro define-entry-point (system-name () &body body)
  "Define the application's entry point."
  #|
  (let ((entry-point (intern (symbol-name system-name)
                             (find-package :ceramic-entry)))
        (arguments (gensym))
        (electron-directory (gensym)))
    `(defun ,entry-point (,arguments)
       (declare (ignore ,arguments))
       ;; Start the executable-relative Electron process
       (let* ((*releasep* t)
              (,electron-directory (executable-relative-pathname #p"electron/"))
              (*process*
                (progn
                  (format t "~&Starting Electron process...")
                  (ceramic.electron:start-process ,electron-directory
                                                  :operating-system *operating-system*))))
         (handler-case
             (progn
               ,@body
                 (dispatch-events))
           (t () (quit)))
  (quit)))))|#
  `())
