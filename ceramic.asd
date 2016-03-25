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
               :trivial-exe
               :trivial-build
               :trivial-compress
               :electron-tools
               :uiop
               :jonathan
               :cl-json
               :external-program
               :uuid
               :copy-directory
               :clack-handler-hunchentoot
               :remote-js)
  :components ((:module "src"
                :serial t
                :components
                ((:file "error")
                 (:file "os")
                 (:file "file")
                 (:file "logging")
                 (:file "runtime")
                 (:file "resource")
                 (:file "setup")
                 (:module "electron"
                  :serial t
                  :components
                  ((:static-file "main.js")
                   (:file "driver")))
                 (:file "driver")
                 #-quicklisp
                 (:file "ql-patch")
                 #+quicklisp
                 (:file "bundler")
                 (:file "ceramic"))))
  :description "Common Lisp web apps on the desktop"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op ceramic-test))))
