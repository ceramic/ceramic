(defsystem electron-tools
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
               :osicat)
  :components ((:module "src"
                :serial t
                :components
                ((:file "electron-tools"))))
  :description "Download, extract, and run Electron binaries."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op electron-tools-test))))
