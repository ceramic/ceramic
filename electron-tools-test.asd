(defsystem electron-tools-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:electron-tools
               :trivial-extract
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "electron-tools")))))
