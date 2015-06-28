(in-package :cl-user)
(defpackage ceramic.electron
  (:use :cl)
  (:import-from :ceramic.electron.tools
                :binary-pathname)
  (:export :start-process
           :send-command)
  ;; Commands
  (:export :create-window
           :window-load-url
           :show-window
           :quit))
(in-package :ceramic.electron)

(defun start-process (directory &key operating-system)
  "Start an Electron process, returning the process object."
  (let ((binary-pathname (binary-pathname directory
                                          :operating-system operating-system)))
    (external-program:start binary-pathname
                            (list)
                            :input :stream
                            :output :stream)))

(defun send-command (process command alist)
  "Send a command Electron process."
  (let ((json-string (cl-json:encode-json-alist-to-string
                      (append (list (cons "cmd" command))
                              alist)))
        (input-stream (external-program:process-input-stream process)))
    (format t "Sending ~A~%" json-string)
    (write-string json-string input-stream)
    (write-char #\Newline input-stream)
    (finish-output input-stream)
    json-string))

;;; Commands

(defun create-window (process name)
  (send-command process "create-window"
                (list (cons "name" name))))

(defun window-load-url (process name url)
  (send-command process "load-url"
                (list (cons "name" name)
                      (cons "url" url))))

(defun show-window (process name)
  (send-command process "show-window"
                (list (cons "name" name))))

(defun quit (process)
  (send-command process "quit" nil))
