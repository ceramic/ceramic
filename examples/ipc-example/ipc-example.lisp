(in-package :cl-user)
(defpackage ceramic-ipc-example
  (:use :cl :lucerne)
  (:import-from :ceramic.resource
                :define-resources
                :resource-directory
                :resource)
  (:export :run))
(in-package :ceramic-ipc-example)
(annot:enable-annot-syntax)

;; Resources

(define-resources :ceramic-ipc-example ()
  (files #p"files/"))

;; Define an application

(defapp app
  :middlewares ((clack.middleware.static:<clack-middleware-static>
                 :path "/static/"
                 :root (resource-directory 'files))))

;; Templates

(defvar +index+
  (djula:compile-template* (resource 'files #p"template.html")))

;; Route requests to "/" to this function
@route app "/"
(defview hello ()
  (render-template (+index+)))

(defvar *port* 8001)

(defvar *window*)

(setf ceramic:*event-dispatcher*
      (lambda (event)
        (let ((event-type (getf event :|event|)))
          (format t "Got event of type ~S~%" event-type)
          (when (string= event-type "async")
            (ceramic:send-message *window*
                                  (list (cons "text" "received!")))))))

(defun run ()
  (let ((*window* (ceramic:make-window :url (format nil "http://localhost:~D/" *port*))))
    (start app :port *port*)
    (ceramic:show-window *window*)
    (ceramic:open-dev-tools *window*)
    (ceramic::dispatch-events)))

(ceramic:define-entry-point :ceramic-hello-world ()
  (run))
