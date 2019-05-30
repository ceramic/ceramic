(in-package :cl-user)
(defpackage ceramic.window
  (:use :cl)
  (:shadow :close)
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
  (:documentation "The window class and its implementation."))
(in-package :ceramic.window)

;;; Utilities

(defun js (format-control &rest args)
  (ceramic.driver:js *driver* (apply #'format (cons nil (cons format-control args)))))

(defun sync-js (format-control &rest args)
  (ceramic.driver:sync-js *driver* (apply #'format (cons nil (cons format-control args)))))

;;; Classes

(defclass window ()
  ((%id :reader window-id
        :initform (uuid:format-as-urn nil (uuid:make-v4-uuid))
        :type string
        :documentation "A unique string ID for the window."))
  (:documentation "A browser window."))

(defun make-window (&key title url width height frame show transparent resizable (class 'window))
  "Create a window."
  (flet ((remove-null-values (plist)
           (loop for (key value) on plist by #'cddr
                 if value
                 appending (list key value))))
    (let ((options (cl-json:encode-json-plist-to-string
                    (remove-null-values
                     (list :title title
                           :width width
                           :height height
			   :frame frame
			   :show show
			   :transparent transparent
			   :resizable resizable))))
          (win (make-instance class)))
      (with-slots (%id) win
        (js "Ceramic.windows[~S] = Ceramic.createWindow(~S, ~A)" %id (or url "null") options))
      win)))

;;; Methods

(defmacro define-trivial-operation (name js &key docstring sync)
  `(defmethod ,name ((window window))
     ,docstring
     (with-slots (%id) window
       ,(if sync
            `(sync-js "return Ceramic.windows[~S].~A" %id ,js)
            `(progn (js "Ceramic.windows[~S].~A" %id ,js)
                    nil)))))

;;; Predicates

(define-trivial-operation loadingp "isLoading"
  :docstring "Return whether the window is loading a new page."
  :sync t)

(define-trivial-operation crashedp "webContents.isCrashed()"
  :docstring "Return whether the window has crashed."
  :sync t)

;;; Getters

(define-trivial-operation title "getTitle()"
  :docstring "Return the window's title."
  :sync t)

(define-trivial-operation url "webContents.getURL()"
  :docstring "Return the window's current URL."
  :sync t)

;;; Setters

(defmethod (setf title) (new-value (window window))
  "Set the window's title."
  (with-slots (%id) window
    (js "Ceramic.windows[~S].setTitle(~S)" %id new-value))
  new-value)

(defmethod (setf url) (new-value (window window))
  "Change the window's URL."
  (with-slots (%id) window
    (js "Ceramic.windows[~S].loadURL(~S)" %id new-value))
  new-value)

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

(define-trivial-operation %reload-force "webContents.reloadIgnoringCache()")

(defmethod reload ((window window) &key ignore-cache)
  "Reload the window. Optionally ignore the cache."
  (if ignore-cache
      (%reload window)
      (%reload-force window)))

(define-trivial-operation stop-loading "webContents.stop()"
  :docstring "Stop any navigation.")

(define-trivial-operation back "webContents.goBack()"
  :docstring "Go back in the page history.")

(define-trivial-operation forward "webContents.goForward()"
  :docstring "Go forward in the page history.")

(define-trivial-operation undo "webContents.undo()"
  :docstring "Undo changes.")

(define-trivial-operation redo "webContents.redo()"
  :docstring "Redo changes.")

(define-trivial-operation cut "webContents.cut()"
  :docstring "Cut selected text.")

(define-trivial-operation copy "webContents.copy()"
  :docstring "Copy selected text.")

(define-trivial-operation paste "webContents.paste()"
  :docstring "Paste text.")

(define-trivial-operation select-all "webContents.selectAll()"
  :docstring "Select all text.")

(define-trivial-operation unselect "webContents.unselect()"
  :docstring "Cancel the selection.")

(define-trivial-operation open-dev-tools "webContents.openDevTools()"
  :docstring "Open the developer tools.")

(define-trivial-operation close-dev-tools "webContents.closeDevTools()"
  :docstring "Close the developer tools.")
