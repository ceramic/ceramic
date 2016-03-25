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

;;; Window class

(defun make-window (&rest args
                    &key title url x y width height
                      min-width min-height max-width max-height
                      resizablep node-integration-p)
  "Create a window."
  (declare (ignore title url x y width height
                   min-width min-height max-width max-height
                   resizablep node-integration-p))
  (apply #'make-instance 'window args))

;;; Setters

(defmacro call-with-defaults (function window &rest args)
  "Call a ceramic.electron command with the *process* object."
  `(,function *process* (window-id ,window) ,@args))

(defmethod (setf window-title) (title (window window))
  (call-with-defaults ceramic.electron:set-window-title
                      window
                      title)
  (setf (slot-value window 'title) title))

(defmethod (setf window-url) (url (window window))
  (call-with-defaults ceramic.electron:window-load-url
                      window
                      url)
  (setf (slot-value window 'url) url))

(defmethod (setf window-x) (x (window window))
  (call-with-defaults ceramic.electron:set-window-position
                      window
                      x
                      (window-y window))
  (setf (slot-value window 'x) x))

(defmethod (setf window-y) (y (window window))
  (call-with-defaults ceramic.electron:set-window-position
                      window
                      (window-x window)
                      y)
  (setf (slot-value window 'y) y))

(defmethod (setf window-width) (width (window window))
  (call-with-defaults ceramic.electron:resize-window
                      window
                      width
                      (window-height window))
  (setf (slot-value window 'width) width))

(defmethod (setf window-height) (height (window window))
  (call-with-defaults ceramic.electron:resize-window
                      window
                      (window-width window)
                      height)
  (setf (slot-value window 'height) height))

(defmethod (setf window-resizable-p) (resizable (window window))
  (if resizable
      (call-with-defaults ceramic.electron:resizable-window
                          window)
      (call-with-defaults ceramic.electron:unresizable-window
                          window))
  (setf (slot-value window 'resizablep) resizable))

;;; Methods

(defmethod initialize-instance :after ((window window) &key)
  "Create the window in the Electron process."
  (macrolet ((slot (name string)
               `(if (and (slot-boundp window ',name)
                         (not (null (slot-value window ',name))))
                    (list (cons ,string (slot-value window ',name))))))
    (call-with-defaults ceramic.electron:create-window
                        window
                        (append
                         (slot title "title")
                         (slot x "x")
                         (slot y "y")
                         (slot width "width")
                         (slot height "height")
                         (slot min-width "min-width")
                         (slot min-height "min-height")
                         (slot max-width "max-width")
                         (slot max-height "max-height")
                         (list (cons "node-integration"
                                     (cl-json:json-bool (node-integration-p window))))
                         (list (cons "resizable"
                                     (cl-json:json-bool (window-resizable-p window))))))
    (when (slot-boundp window 'url)
      (call-with-defaults ceramic.electron:window-load-url
                          window
                          (window-url window)))))

(defmethod show-window ((window window))
  "Show the window."
  (call-with-defaults ceramic.electron:show-window
                      window))

(defmethod hide-window ((window window))
  "Hide the window."
  (call-with-defaults ceramic.electron:hide-window
                      window))

(defmethod close-window ((window window))
  "Close the window."
  (call-with-defaults ceramic.electron:close-window
                      window))

(defmethod destroy-window ((window window))
  "Forcefully close the window."
  (call-with-defaults ceramic.electron:destroy-window
                      window))

(defmethod send-message ((window window) message)
  "Send an alist message to the window."
  (call-with-defaults ceramic.electron:send-message
                      window
                      message))

(defmethod maximize-window ((window window))
  "Maximize the window."
  (call-with-defaults ceramic.electron:maximize-window
                      window))

(defmethod unmaximize-window ((window window))
  "Un-maximize the window."
  (call-with-defaults ceramic.electron:unmaximize-window
                      window))

(defmethod minimize-window ((window window))
  "Minimize the window."
  (call-with-defaults ceramic.electron:minimize-window
                      window))

(defmethod unminimize-window ((window window))
  "Un-minimize the window."
  (call-with-defaults ceramic.electron:unminimize-window
                      window))

(defmethod center-window ((window window))
  "Center the window."
  (call-with-defaults ceramic.electron:center-window
                      window))

(defmethod open-dev-tools ((window window))
  "Open the dev tools."
  (call-with-defaults ceramic.electron:window-open-dev-tools
                      window))

(defun quit (&optional (exit-status 0))
  "Quit the application."
  (ceramic.driver:stop ceramic.driver:*driver*)
  (uiop:quit exit-status))

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
