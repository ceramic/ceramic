(defsystem ceramic-electron-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:ceramic-electron
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "ceramic-electron")))))
