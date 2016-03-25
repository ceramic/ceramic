(in-package :cl-user)
(defpackage ceramic
  (:use :cl)
  (:import-from :ceramic.os
                :*operating-system*)
  (:import-from :ceramic.runtime
                :*releasep*
                :executable-relative-pathname)
  (:import-from :ceramic.electron
                :start-process)
  (:import-from :electron-tools
                :binary-pathname)
  (:import-from :ceramic.file
                :release-directory)
  ;; Main interface
  (:export :setup
           :interactive
           :stop-interactive
           :with-interactive
           :define-entry-point
           :quit)
  ;; Window & accessors
  (:export :window
           :make-window
           :window-title
           :window-url
           :window-x
           :window-y
           :window-width
           :window-height
           :window-resizable-p)
  ;; Methods
  (:export :show-window
           :hide-window
           :close-window
           :destroy-window
           :send-message
           :maximize-window
           :unmaximize-window
           :minimize-window
           :unminimize-window
           :center-window
           :open-dev-tools)
  ;; Events
  (:export :*event-dispatcher*)
  (:documentation "The main interface."))
(in-package :ceramic)

#|
;;; Entry point for released applications

(defpackage ceramic-entry
  (:use :cl))

(defmacro define-entry-point (system-name () &body body)
  "Define the application's entry point."
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
         (quit)))))
|#
