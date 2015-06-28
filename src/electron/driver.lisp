(in-package :cl-user)
(defpackage ceramic.electron
  (:use :cl)
  (:import-from :ceramic.electron.tools
                :binary-pathname)
  (:export :start-process
           :send-command)
  ;; Commands
  (:export :quit))
(in-package :ceramic.electron)

(defparameter +main-javascript+
  (asdf:system-relative-pathname :ceramic-electron #p"src/main.js")
  "Pathname to the JavaScript file for the main process.")

(defun clean-release (directory)
  "Clean up default files from an Electron release."
  (let ((app-files (list #p"main.js"
                         #p"default_app.js"
                         #p"index.html"
                         #p"package.json")))
    (loop for file in app-files do
      (let ((pathname (merge-pathnames file
                                       (merge-pathnames #p"resources/default_app/"
                                                        directory))))
        (delete-file pathname)))))

(defun insert-javascript (directory)
  "Insert the main process JavaScript into an Electron release."
  (uiop:copy-file +main-javascript+
                  (merge-pathnames #p"resources/default_app/main.js"
                                   directory)))

(defun insert-package-definition (directory &key name version)
  "Insert the package.json into an Electron release."
  (with-open-file (output-stream (merge-pathnames #p"resources/default_app/package.json"
                                                  directory)
                                 :direction :output
                                 :if-does-not-exist :create)
    (write-string (jonathan:to-json (list (cons "name" name)
                                          (cons "version" version)
                                          (cons "main" "main.js"))
                                    :from :alist)
                  output-stream)))

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
