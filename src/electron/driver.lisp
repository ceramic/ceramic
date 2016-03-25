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

(defun start-process (directory &key operating-system)
  "Start an Electron process, returning the process object."
  (let* ((binary-pathname (binary-pathname directory
                                           :operating-system operating-system))
         (process (external-program:start binary-pathname
                                          (list)
                                          :input :stream
                                          :output :stream)))
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
