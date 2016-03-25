(in-package :cl-user)
(defpackage ceramic.electron
  (:use :cl)
  (:import-from :ceramic.file
                :*ceramic-directory*)
  (:import-from :ceramic.os
                :*operating-system*
                :*architecture*)
  (:import-from :electron-tools
                :binary-pathname
                :get-release)
  (:import-from :ceramic.electron.tools
                :prepare-release)
  ;; Interface
  (:export :*electron-version*
           :release-directory
           :global-binary-pathame
           :setup)
  ;; Functions
  (:export :*binary-pathname*
           :start-process
           :send-command)
  ;; Commands
  (:export :create-window
           :close-window
           :destroy-window
           :send-message
           :show-window
           :hide-window
           :resize-window
           :focus-window
           :maximize-window
           :unmaximize-window
           :minimize-window
           :unminimize-window
           :fullscreen-window
           :unfullscreen-window
           :resizable-window
           :unresizable-window
           :center-window
           :set-window-position
           :set-window-title
           :window-load-url
           :window-reload
           :window-open-dev-tools
           :window-close-dev-tools
           :window-undo
           :window-redo
           :window-cut
           :window-paste
           :window-delete
           :window-select-all
           :quit))
(in-package :ceramic.electron)

(defun wait-for-startup (process)
  "Wait until the Electron process has started and is ready to go."
  (labels ((read-all-from-stream (stream)
             (concatenate 'string
                          (loop for byte = (read-char-no-hang stream nil nil)
                                while byte collecting byte)))
           (process-stdout ()
             (read-all-from-stream
              (external-program:process-output-stream
               process))))
    (ceramic.log:log-message "Waiting for startup...")
    (let ((output (process-stdout)))
      (loop until (search "READY" output) do
        (let ((new-output (process-stdout)))
          (setf output (concatenate 'string output new-output)))))
    ;; Clear all stdout
    (process-stdout)
    (ceramic.log:log-message "Electron process started.")
    t))

(defun start-process (directory &key operating-system)
  "Start an Electron process, returning the process object."
  (let* ((binary-pathname (binary-pathname directory
                                           :operating-system operating-system))
         (process (external-program:start binary-pathname
                                          (list)
                                          :input :stream
                                          :output :stream)))
    (wait-for-startup process)
    process))

(defun send-command (process command alist)
  "Send a command Electron process."
  (let ((json-string (cl-json:with-explicit-encoder
                       (cl-json:encode-json-alist-to-string
                        (append (list (cons "cmd" command))
                                alist))))
        (input-stream (external-program:process-input-stream process)))
    (write-string json-string input-stream)
    (write-char #\Newline input-stream)
    (finish-output input-stream)
    (ceramic.log:log-message "sent JSON ~A" json-string)
    json-string))

;;; Commands

(defmacro define-window-command (name string (&rest args) &body alist)
  `(defun ,name (process name ,@args)
     (send-command process
                   ,string
                   (append (list (cons "name" name))
                           (progn
                             ,@alist)))))

(define-window-command create-window "create-window" (options)
  (append (list (cons "show" (cl-json:json-bool nil)))
          options))

(define-window-command close-window "close-window" ()
  ())

(define-window-command destroy-window "destroy-window" ()
  ())

(define-window-command send-message "send-message-to-window" (message)
  message)

(define-window-command show-window "show-window" ()
  ())

(define-window-command hide-window "hide-window" ()
  ())

(define-window-command resize-window "resize-window" (width height)
  (list (cons "width" width)
        (cons "height" height)))

(define-window-command focus-window "focus-window" ()
  ())

(define-window-command maximize-window "maximize-window" ()
  ())

(define-window-command unmaximize-window "unmaximize-window" ()
  ())

(define-window-command minimize-window "minimize-window" ()
  ())

(define-window-command unminimize-window "unminimize-window" ()
  ())

(define-window-command fullscreen-window "fullscreen-window" ()
  ())

(define-window-command unfullscreen-window "unfullscreen-window" ()
  ())

(define-window-command resizable-window "resizable-window" ()
  ())

(define-window-command unresizable-window "unresizable-window" ()
  ())

(define-window-command center-window "center-window" ()
  ())

(define-window-command set-window-position "set-window-position" (x y)
  (list (cons "x" x)
        (cons "y" y)))

(define-window-command set-window-title "set-window-title" (title)
  (list (cons "title" title)))

(define-window-command window-load-url "window-load-url" (url)
  (list (cons "url" url)))

(define-window-command window-reload "window-reload" ()
  ())

(define-window-command window-open-dev-tools "window-open-dev-tools" ()
  ())

(define-window-command window-close-dev-tools "window-close-dev-tools" ()
  ())

(define-window-command window-undo "window-undo" ()
  ())

(define-window-command window-redo "window-redo" ()
  ())

(define-window-command window-cut "window-cut" ()
  ())

(define-window-command window-copy "window-copy" ()
  ())

(define-window-command window-paste "window-paste" ()
  ())

(define-window-command window-delete "window-delete" ()
  ())

(define-window-command window-select-all "window-select-all" ()
  ())

(defun quit (process)
  "End the Electron process."
  (send-command process "quit" nil))

;;; Interface

(defvar *electron-version* "0.28.1"
  "The version of Electron to use.")

(defun release-directory ()
  "Pathname to the local copy of the Electron release."
  (merge-pathnames #p"electron/" *ceramic-directory*))

(defun global-binary-pathname ()
  "The pathname to the downloaded Electron binary. Used for interactive
  testing."
  (binary-pathname (release-directory)
                   :operating-system *operating-system*))

(defun setup ()
  "Set up the Electron driver."
  (ensure-directories-exist (release-directory))
  (progn
    (get-release (release-directory)
                 :operating-system *operating-system*
                 :architecture *architecture*
                 :version *electron-version*)
    (prepare-release (release-directory) :operating-system *operating-system*)))
