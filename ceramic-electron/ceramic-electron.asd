(defsystem ceramic-electron
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :depends-on (:electron-tools
               :jonathan
               :cl-json
               :external-program)
  :components ((:module "src"
                :serial t
                :components
                ((:static-file "main.js")
                 (:file "ceramic-electron"))))
  :description "Electron driver for Ceramic."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op ceramic-electron-test))))
