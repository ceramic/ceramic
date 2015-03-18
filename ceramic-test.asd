(defsystem ceramic-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:ceramic
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "download")
                 (:file "ceramic")))))
