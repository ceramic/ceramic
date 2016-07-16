(in-package :cl-user)
(defpackage ceramic
  (:use :cl)
  (:import-from :ceramic.driver
                :*driver*)
  (:import-from :ceramic.setup
                :setup)
  (:import-from :ceramic.runtime
                :*releasep*)
  (:import-from :ceramic.crashreporter
                :start-crash-reporter)
  #+quicklisp
  (:import-from :ceramic.bundler
                :bundle)
  (:import-from :ceramic.resource
                :define-resources
                :resource-directory
                :resource)
  (:import-from :ceramic.window
                :window
                :window-id
                :make-window
                ;; Predicates
                :loadingp
                :crashedp
                ;; Accessors
                :title
                :url
                ;; Operations
                :show
                :hide
                :center
                :reload
                :stop-loading
                :back
                :forward
                :undo
                :redo
                :cut
                :copy
                :paste
                :select-all
                :unselect
                :open-dev-tools
                :close-dev-tools)
  (:shadowing-import-from :ceramic.window
                          :close)
  (:export :window
           :window-id
           :make-window
           ;; Predicates
           :loadingp
           :crashedp
           ;; Accessors
           :title
           :url
           ;; Operations
           :show
           :hide
           :close
           :center
           :reload
           :stop-loading
           :back
           :forward
           :undo
           :redo
           :cut
           :copy
           :paste
           :select-all
           :unselect
           :open-dev-tools
           :close-dev-tools)
  (:export :start-crash-reporter)
  (:export :bundle)
  (:export :define-resources
           :resource-directory
           :resource)
  (:export :setup
           :start
           :stop
           :quit
           :define-entry-point)
  (:documentation "The main interface."))
(in-package :ceramic)

;;; FIXME move this someone more semantic

#-quicklisp
(defun bundle ()
  nil)

;;; Lifecycle

(defun start ()
  "Start the Electron process."
  (ceramic.driver:start *driver*))

(defun stop ()
  "Stop the Electron process."
  (ceramic.driver:stop *driver*))

;;; Entry point for released applications

(defpackage ceramic-entry
  (:use :cl))

(defmacro define-entry-point (system-name () &body body)
  "Define the application's entry point."
  (let ((entry-point (intern (symbol-name system-name)
                             (find-package :ceramic-entry))))
    `(defun ,entry-point ()
       ;; Start the executable-relative Electron process
       (let ((*releasep* t))
         (start)
         (handler-case
             (progn
               ,@body
               (loop (sleep 1)))
           (t () (quit)))))))
