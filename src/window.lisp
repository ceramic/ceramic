(in-package :cl-user)
(defpackage ceramic.window
  (:use :cl)
  (:import-from :ceramic.driver
                :*driver*)
  (:documentation "The window class and its implementation."))
(in-package :ceramic.window)

;;; Classes

(defclass window ()
  ((id :reader window-id
       :initform (uuid:format-as-urn nil (uuid:make-v4-uuid))
       :type string
       :documentation "A unique string ID for the window."))
  (:documentation "A browser window."))

;;; Methods

(defun js (format-control &rest args)
  (ceramic.driver:sync-js *driver* (apply #'format (cons nil (cons format-control args)))))

(defmacro window-js (id &rest args)
  `(js "Ceramic.windows[~S].~A" ,id ,@args))

(defmacro define-trivial-operation (name js &key docstring)
  (alexandria:with-gensyms (window id)
    `(defmethod ,name ((,window window))
       ,docstring
       (with-slots (,id) ,window
         (window-js ,js ,id)))))

;;; Getters

(define-trivial-operation window-title "getTitle()"
  :docstring "Return the window's title.")

(define-trivial-operation window-url "webContents.getURL()"
  :docstring "Return the window's current URL.")

(define-trivial-operation center "center()"
  :docstring "Move the window to the center of the screen.")

(define-trivial-operation %reload "reload()")

(define-trivial-operation %reload-force "reloadIgnoringCache()")

(defmethod reload ((window window) &key ignore-cache)
  "Reload the window. Optionally ignore the cache."
  (if ignore-cache
      (%reload window)
      (%reload-force window)))

(define-trivial-operation loadingp "isLoading"
  :docstring "Return whether the window is loading a new page.")

(define-trivial-operation stop "stop()"
  :docstring "Stop any navigation.")

(define-trivial-operation back "goBack()"
  :docstring "Go back in the page history.")

(define-trivial-operation forward "goForward()"
  :docstring "Go forward in the page history.")

(define-trivial-operation crashedp "isCrashed()"
  :docstring "Return whether the window has crashed.")

(define-trivial-operation undo "undo()"
  :docstring "Undo changes.")

(define-trivial-operation redo "redo()"
  :docstring "Redo changes.")

(define-trivial-operation cut "cut()"
  :docstring "Cut selected text.")

(define-trivial-operation copy "copy()"
  :docstring "Copy selected text.")

(define-trivial-operation paste "paste()"
  :docstring "Paste text.")

(define-trivial-operation select-all "selectAll()"
  :docstring "Select all text.")

(define-trivial-operation unselect "unselect()"
  :docstring "Cancel the selection.")

(define-trivial-operation open-dev-tools "openDevTools()"
  :docstring "Open the developer tools.")

(define-trivial-operation close-dev-tools "closeDevTools()"
  :docstring "Close the developer tools.")

;;; Setters

(defmethod (setf window-title) (new-value (window window))
  "Set the window's title."
  (with-slots (id) window
    (window-js "setTitle(~S)" id new-value)))

(defmethod (setf window-url) (new-value (window window))
  "Change the window's URL."
  (with-slots (id) window
    (window-js "loadURL(~S") id new-value))
