(in-package :cl-user)
(defpackage ceramic-electron-test
  (:use :cl :fiveam))
(in-package :ceramic-electron-test)

;;; Utilities

(defvar *test-directory*
  (asdf:system-relative-pathname :ceramic-electron-test #p"t/test/"))

(defun current-os ()
  "Return an (operating system, architecture) pair, e.g. @c((:linux . :64)),
@c((:windows . :32))."
  (let ((64-bit (eql (cffi:foreign-type-size '(:pointer :int)) 8)))
    (cons (cond
            ((uiop:os-windows-p)
             :windows)
            ((uiop:os-macosx-p)
             :mac)
            ((eq (uiop:operating-system) :linux)
             :linux))
          (if 64-bit
              :64
              :32))))

;;; Tests

(def-suite tests
  :description "ceramic-electron tests.")
(in-suite tests)

(test cleanup
  (finishes
    (when (probe-file *test-directory*)
      (uiop:delete-directory-tree *test-directory* :validate t))))

(test download-electron
  (let ((snapshot (merge-pathnames #p"electron.zip"
                                   *test-directory*)))
    (finishes
      (electron-tools:download snapshot
                               :operating-system :linux
                               :version "0.28.1"
                               :architecture :64))
    (finishes
      (electron-tools:extract snapshot))
    ;; Delete the snapshot archive
    (delete-file snapshot)))

(test changes
  (finishes
    (ceramic-electron::clean-release *test-directory*))
  (finishes
    (ceramic-electron::insert-javascript *test-directory*))
  (finishes
    (ceramic-electron::insert-package-definition *test-directory*
                                                 :name "test"
                                                 :version "0.1")))

(defparameter *process* nil)

(test start-app
  (finishes
    (setf *process* (ceramic-electron:start-process *test-directory*
                                                    :operating-system :linux)))
  (sleep 0.1)
  (finishes
    (print (read-line (external-program:process-output-stream *process*)))))

(test windows
  (finishes
    (ceramic-electron::create-window *process* "win"))
  (finishes
    (ceramic-electron::window-load-url *process* "win" "https://www.google.com/"))
  (sleep 5))

(test stop-app
  (finishes
    (ceramic-electron:quit *process*))
  (sleep 1)
  (is
   (equal (external-program:process-status *process*)
          :exited)))

(run! 'tests)
