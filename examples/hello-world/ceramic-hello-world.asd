(defsystem ceramic-hello-world
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:ceramic
               :lucerne)
  :components ((:file "hello-world"))
  :description "A Hello World example for Ceramic.")
