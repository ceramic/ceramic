(defsystem ceramic-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:ceramic
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:module "electron"
                  :serial t
                  :components
                  ((:file "tools")
                   (:file "driver")))
                 (:file "setup")
                 (:file "integration")
                 (:file "ceramic")))))
