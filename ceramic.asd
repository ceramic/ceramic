(defsystem ceramic
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage "http://ceramic.github.io/"
  :bug-tracker "https://github.com/ceramic/ceramic/issues"
  :source-control (:git "git@github.com:ceramic/ceramic.git")
  :depends-on (:trivial-download
               :trivial-extract
               #-(or win32 mswindows)
               :osicat
               :jonathan
               :cl-json
               :external-program
               :buildapp
               :uuid
               :zip
               :ceramic
               :cl-fad)
  :components ((:module "src"
                :serial t
                :components
                ((:file "util")
                 (:file "error")
                 (:file "os")
                 (:file "file")
                 (:file "runtime")
                 (:file "resource")
                 (:module "electron"
                  :serial t
                  :components
                  ((:file "tools")
                   (:static-file "main.js")
                   (:file "driver")))
                 (:file "setup")
                 (:file "ceramic")
                 #-ceramic-release
                 (:file "build")
                 #-ceramic-release
                 (:file "bundler"))))
  :description "Common Lisp web apps on the desktop"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op ceramic-test))))
