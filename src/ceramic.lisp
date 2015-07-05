(in-package :cl-user)
(defpackage ceramic
  (:use :cl)
  (:import-from :ceramic.os
                :*operating-system*)
  (:import-from :ceramic.runtime
                :*releasep*)
  (:import-from :ceramic.electron
                :start-process
                :release-directory)
  (:export :interactive)
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
           :destroy-window)
  (:documentation "The main interface."))
(in-package :ceramic)

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
        (setf *process*
              (ceramic.electron:start-process (ceramic.electron:release-directory)
                                              :operating-system *operating-system*))
        t)))

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
   (resizablep :reader window-resizable-p
               :initarg :resizablep
               :initform t
               :type boolean
               :documentation "Whether or not the window is resizable."))
  (:documentation "A browser window."))

(defun make-window (&key title url x y width height resizablep)
  "Create a window."
  (let ((args (remove-if #'(lambda (pair)
                             (null (rest pair)))
                         (list (cons :title title)
                               (cons :url url)
                               (cons :x x)
                               (cons :y y)
                               (cons :width width)
                               (cons :height height)
                               (cons :resizablep resizablep)))))
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
                         (list (cons "resizable" (window-resizable-p window)))))
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

;;; Entry point for released applications

(defpackage ceramic-entry
  (:use :cl))

(defmacro define-entry-point (system-name () &body body)
  (let ((entry-point (intern (symbol-name system-name)
                             (find-package :ceramic-entry)))
        (arguments (gensym)))
    `(defun ,entry-point (,arguments)
       ;; Start the executable-relative Electron process
       ,@body)))
