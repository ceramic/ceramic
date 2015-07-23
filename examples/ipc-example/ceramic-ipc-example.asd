(defsystem ceramic-ipc-example
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:ceramic
               :lucerne)
  :components ((:file "ipc-example"))
  :description "An example of using IPC in Ceramic.")
