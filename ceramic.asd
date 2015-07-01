(defsystem ceramic
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :depends-on (:trivial-download
               :trivial-extract
               #-(or win32 mswindows)
               :osicat
               :jonathan
               :cl-json
               :external-program
               :buildapp)
  :components ((:module "src"
                :serial t
                :components
                ((:file "util")
                 (:file "error")
                 (:file "os")
                 (:file "file")
                 (:module "electron"
                  :serial t
                  :components
                  ((:file "tools")
                   (:static-file "main.js")
                   (:file "driver")))
                 (:file "setup"))))
  :description "Common Lisp web apps on the desktop"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op ceramic-test))))
