(defsystem ceramic-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:ceramic
               :ceramic-test-app
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
                 (:file "misc")
                 (:file "ceramic")))))
