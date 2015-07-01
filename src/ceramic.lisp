(in-package :cl-user)
(defpackage ceramic
  (:use :cl)
  (:import-from :ceramic.os
                :*operating-system*)
  (:import-from :ceramic.electron
                :start-process
                :release-directory)
  (:export :interactive)
  ;; Window class
  (:export :window
           :window-title
           :window-x
           :window-y
           :window-width
           :window-height
           :window-resizable-p)
  (:documentation "The main interface."))
(in-package :ceramic)

;;; Process management

(defvar *process* nil
  "The Electron process.")

(defun interactive ()
  "Start a process for interactive use."
  (setf *process*
        (ceramic.electron:start-process (ceramic.electron:release-directory)
                                        :operating-system *operating-system*)))

;;; Window class

(defclass window ()
  ((id :reader window-id
       :initform (uuid:make-v4-uuid)
       :type string
       :documentation "A unique string ID for the window.")
   (title :reader window-title
          :initarg :title
          :type string
          :documentation "The window title.")
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

(defun make-window (&key title x y width height resizablep)
  (make-instance 'window
                 :title title
                 :x x
                 :y y
                 :width width
                 :height height
                 :resizablep resizablep))

;;; Setters

(defmacro call-with-defaults (function window &rest args)
  "Call a ceramic.electron command with the *process* object."
  `(,function *process* (window-id ,window) ,@args))

(defmethod (setf window-title) (title (window window))
  (call-with-defaults ceramic.electron:set-window-title
                      window
                      title)
  (setf (slot-value window 'title) title))

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
                         (cons "resizable" (window-resizable-p window))))))

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
