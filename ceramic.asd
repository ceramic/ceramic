(defsystem ceramic
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :depends-on (:trivial-types
               :trivial-download
               :trivial-extract
               :anaphora)
  :components ((:module "src"
                :serial t
                :components
                ((:file "error")
                 (:file "download"))))
  :description "Common Lisp web apps on the desktop"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op ceramic-test))))
