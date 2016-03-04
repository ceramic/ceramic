(in-package :cl-user)
(defpackage ceramic
  (:use :cl)
  (:import-from :ceramic.os
                :*operating-system*)
  (:import-from :ceramic.runtime
                :*releasep*
                :executable-relative-pathname)
  (:import-from :ceramic.electron
                :start-process
                :release-directory)
  (:import-from :ceramic.electron.tools
                :binary-pathname)
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

;;; Setup

(defun setup ()
  "Set up everything needed for Ceramic to run."
  (ceramic.setup:setup))

;;; Process management

(defvar *process* nil
  "The Electron process.")

(defun interactive ()
  "Start a process for interactive use."
  (if *releasep*
      ;; We're in a release, so don't let the user do this
      nil
      ;; We're in a dev environment
      (progn
        (when *process*
          (warn "Interactive process already running. Restarting.")
          (stop-interactive))
        (setf *process*
              (ceramic.electron:start-process (ceramic.electron:release-directory)
                                              :operating-system *operating-system*))
        t)))

(defun stop-interactive ()
  "Stop the interactive process."
  (handler-case
      (ceramic.electron:quit *process*)
    (t ()
      (warn "Error quitting the Electron process. Forcing shutdown...")
      (external-program:signal-process *process* :killed))))

(defmacro with-interactive (() &body body)
  "Execute body while running an interactive process."
  `(unwind-protect
        (progn
          (interactive)
          ,@body)
     (stop-interactive)))

;;; Events

(defvar *event-dispatcher*
  (lambda (event)
    (declare (ignore event))
    nil)
  "A function that takes an event (A JSON object) and does something with it.")

;;; Window class

(defclass window ()
  ((id :reader window-id
       :initform (uuid:format-as-urn nil (uuid:make-v4-uuid))
       :type string
       :documentation "A unique string ID for the window.")
   (title :reader window-title
          :initarg :title
          :type string
          :documentation "The window title.")
   (url :reader window-url
        :initarg :url
        :type string
        :documentation "The window URL.")
   ;; Window geometry
   (x :reader window-x
      :initarg :x
      :type integer
      :documentation "Window's offset from the left.")
   (y :reader window-y
      :initarg :y
      :type integer
      :documentation "Window's offset from the top.")
   (width :reader window-width
          :initarg :width
          :type integer
          :documentation "The window's width, in pixels.")
   (height :reader window-height
           :initarg :height
           :type integer
           :documentation "The window's height, in pixels.")
   (min-width :reader window-min-width
              :initarg :min-width
              :type integer
              :documentation "The window's minimum width, in pixels.")
   (min-height :reader window-min-height
               :initarg :min-height
               :type integer
               :documentation "The window's minimum height, in pixels.")
   (max-width :reader window-max-width
              :initarg :max-width
              :type integer
              :documentation "The window's maximum width, in pixels.")
   (max-height :reader window-max-height
               :initarg :max-height
               :type integer
               :documentation "The window's maximum height, in pixels.")
   (node-integration-p :reader node-integration-p
                       :initarg :node-integration-p
                       :initform nil
                       :type boolean
                       :documentation "Whether node integration is enabled.")
   (resizablep :reader window-resizable-p
               :initarg :resizablep
               :initform t
               :type boolean
               :documentation "Whether or not the window is resizable."))
  (:documentation "A browser window."))

(defun make-window (&key title url x y width height
                      min-width min-height max-width max-height
                      resizablep node-integration-p)
  "Create a window."
  (let ((args (list*
               (cons :resizablep resizablep)
               (cons :node-integration-p node-integration-p)
               (remove-if #'(lambda (pair)
                              (null (rest pair)))
                          (list (cons :title title)
                                (cons :url url)
                                (cons :x x)
                                (cons :y y)
                                (cons :width width)
                                (cons :height height)
                                (cons :min-width min-width)
                                (cons :min-height min-height)
                                (cons :max-width max-width)
                                (cons :max-height max-height))))))
    (apply #'make-instance
           (cons 'window
                 (loop for (key . value) in args appending
                   (list key value))))))

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
  (ceramic.electron:quit *process*)
  (uiop:quit exit-status))

;;; Entry point for released applications

(defpackage ceramic-entry
  (:use :cl))

(defun dispatch-events ()
  "Read events from the process stdout."
  (format t "~&Dispatching events")
  (loop for string = (read-line (external-program:process-output-stream
                                 *process*))
        do
    (when (alexandria:starts-with-subseq "JSON" string)
      (let ((json (jonathan:parse (subseq string 4))))
        (funcall *event-dispatcher* json)))))

(defmacro define-entry-point (system-name () &body body)
  "Define the application's entry point."
  (let ((entry-point (intern (symbol-name system-name)
                             (find-package :ceramic-entry)))
        (arguments (gensym))
        (electron-directory (gensym))
        (binary (gensym)))
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
