(in-package :cl-user)
(defpackage ceramic.window
  (:use :cl)
  (:import-from :ceramic.driver
                :*driver*)
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
           :center
           :reload
           :stop
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
  (:documentation "The window class and its implementation."))
(in-package :ceramic.window)

;;; Utilities

(defun js (format-control &rest args)
  (ceramic.driver:sync-js *driver* (apply #'format (cons nil (cons format-control args)))))

(defmacro window-js (id &rest args)
  `(js "Ceramic.windows[~S].~A" ,id ,@args))

;;; Classes

(defclass window ()
  ((%id :reader window-id
        :initform (uuid:format-as-urn nil (uuid:make-v4-uuid))
        :type string
        :documentation "A unique string ID for the window."))
  (:documentation "A browser window."))

(defun make-window (&key title url)
  "Create a window."
  (let ((options (cl-json:encode-json-plist-to-string
                  (list :title title)))
        (win (make-instance 'window)))
    (with-slots (id) win
      (js "Ceramic.windows[~S] = Ceramic.createWindow(~S, ~A)" id url options))
    win))

;;; Methods

(defmacro define-trivial-operation (name js &key docstring)
  (let ((window (gensym)))
    `(defmethod ,name ((,window window))
       ,docstring
       (with-slots (%id) ,window
         (window-js ,js %id)))))

;;; Predicates

(define-trivial-operation loadingp "isLoading"
  :docstring "Return whether the window is loading a new page.")

(define-trivial-operation crashedp "isCrashed()"
  :docstring "Return whether the window has crashed.")

;;; Getters

(define-trivial-operation title "getTitle()"
  :docstring "Return the window's title.")

(define-trivial-operation url "webContents.getURL()"
  :docstring "Return the window's current URL.")

;;; Setters

(defmethod (setf title) (new-value (window window))
  "Set the window's title."
  (with-slots (id) window
    (window-js "setTitle(~S)" id new-value)))

(defmethod (setf url) (new-value (window window))
  "Change the window's URL."
  (with-slots (id) window
    (window-js "loadURL(~S") id new-value))

;;; Operations

(define-trivial-operation show "show()"
  :docstring "Show the window to the user.")

(define-trivial-operation hide "hide()"
  :docstring "Hide the window from the user.")

(defmethod close ((window window))
  "Close the window."
  (with-slots (%id) window
    (js "Ceramic.closeWindow(~S)" %id)))

(define-trivial-operation center "center()"
  :docstring "Move the window to the center of the screen.")

(define-trivial-operation %reload "reload()")

(define-trivial-operation %reload-force "reloadIgnoringCache()")

(defmethod reload ((window window) &key ignore-cache)
  "Reload the window. Optionally ignore the cache."
  (if ignore-cache
      (%reload window)
      (%reload-force window)))

(define-trivial-operation stop "stop()"
  :docstring "Stop any navigation.")

(define-trivial-operation back "goBack()"
  :docstring "Go back in the page history.")

(define-trivial-operation forward "goForward()"
  :docstring "Go forward in the page history.")

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
