(defsystem ceramic-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:ceramic
               :ceramic-test-app
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "resource")
                 (:file "setup")
                 (:file "driver")
                 (:file "window")
                 (:file "crashreporter")
                 (:file "dialog")
                 (:file "integration")
                 (:file "misc")
                 (:file "ceramic")))))
