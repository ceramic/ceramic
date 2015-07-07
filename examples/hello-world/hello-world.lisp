(in-package :cl-user)
(defpackage ceramic-hello-world
  (:use :cl :lucerne)
  (:export :run))
(in-package :ceramic-hello-world)
(annot:enable-annot-syntax)

;; Define an application
(defapp app)

;; Route requests to "/" to this function
@route app "/"
(defview hello ()
  (respond "Hello, world!"))

(defvar *window* nil)

(defvar *port* 8000)

(defun run ()
  (start app :port *port*)
  (setf *window*
        (ceramic:make-window :url (format nil "http://localhost:~D/" *port*)))
  (ceramic:show-window *window*))

(ceramic:define-entry-point :ceramic-test-app ()
  (run))
