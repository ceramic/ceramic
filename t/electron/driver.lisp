(in-package :cl-user)
(defpackage ceramic-test.electron
  (:use :cl :fiveam)
  (:import-from :external-program
                :process-output-stream
                :process-status)
  (:import-from :ceramic.electron
                :start-process
                :create-window
                :window-load-url
                :quit)
  (:import-from :ceramic-test.electron.tools
                :*test-directory*)
  (:export :tests))
(in-package :ceramic-test.electron)

(defparameter *process* nil)

(def-suite tests
  :description "Electron driver tests.")
(in-suite tests)

(test start-app
  (let ((release (merge-pathnames #p"LINUX64/"
                                  *test-directory*)))
    (finishes
      (setf *process* (start-process release
                                     :operating-system :linux)))
    (sleep 0.1)
    (finishes
      (print (read-line (process-output-stream *process*))))))

(test windows
  (finishes
    (create-window *process* "win" nil))
  (finishes
    (window-load-url *process* "win" "https://www.google.com/"))
  (sleep 5))

(test stop-app
  (finishes
    (quit *process*))
  (sleep 1)
  (is
   (equal (process-status *process*)
          :exited)))

(test cleanup
  (finishes
    (uiop:delete-directory-tree *test-directory* :validate t)))
