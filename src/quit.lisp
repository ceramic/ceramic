(in-package :ceramic)

(defun quit (&optional (exit-status 0))
  "Kill the Electron process and the Lisp process."
  (stop)
  (uiop:quit exit-status))
