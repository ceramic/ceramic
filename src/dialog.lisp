(in-package :cl-user)
(defpackage ceramic.dialog
  (:use :cl)
  (:shadow :close)
  (:import-from :ceramic.window
                :window-id)
  (:import-from :ceramic.driver
                :*driver*
                :sync-js)
  (:export :show-open-dialog
           :show-save-dialog
           :show-message-box
           :show-error-box))
(in-package :ceramic.dialog)

(defun show-standard-dialog (action window options)
  (if window
      (sync-js *driver*
               (format nil
                       "return Ceramic.dialog.~A(Ceramic.windows[~S], ~A);"
                       action
                       (window-id window)
                       (cl-json:encode-json-alist-to-string options)))
      (sync-js *driver*
               (format nil
                       "return Ceramic.dialog.~A(~A);"
                       action
                       (cl-json:encode-json-alist-to-string options)))))

(defun file-filters (spec)
  (loop :for (name . extensions) :in spec
        :collecting `(("name" . ,name)
                      ("extensions" . ,extensions))))

(defun show-open-dialog (&key window title default-path button-label
                              open-file open-directory multi-selections
                              create-directory show-hidden-files
                              filters)
  "Show an Open dialog."
  (let* ((filter-list (file-filters filters))
         (properties (remove nil
                             (list (when open-file "openFile")
                                   (when open-directory "openDirectory")
                                   (when multi-selections "multiSelections")
                                   (when create-directory "createDirectory")
                                   (when show-hidden-files "showHiddenFiles"))))
         (options (remove-if-not #'cdr
                                 `(("title" . ,title)
                                   ("defaultPath" . ,default-path)
                                   ("buttonLabel" . ,button-label)
                                   ("filters" . ,filter-list)
                                   ("properties" . ,properties)))))
    (show-standard-dialog "showOpenDialog" window options)))

(defun show-save-dialog (&key window title default-path button-label filters)
  "Show a Save dialog."
  (let* ((filter-list (file-filters filters))
         (options (remove-if-not #'cdr
                                 `(("title" . ,title)
                                   ("defaultPath" . ,default-path)
                                   ("buttonLabel" . ,button-label)
                                   ("filters" . ,filter-list)))))
    (show-standard-dialog "showSaveDialog" window options)))

(defparameter +message-box-types+ '(nil "none" "info" "error" "question" "warning"))

(defparameter +message-box-default-buttons+ '("OK"))

(defun show-message-box (message &key window type
                                      (buttons +message-box-default-buttons+)
                                      default-id title detail icon cancel-id no-link)
  "Show a message box dialog."
  (declare (ignore icon))
  (assert (member type +message-box-types+ :test #'string=)
          (type) "Unknown message box type ~S" type)
  (let ((options (remove-if-not #'cdr
                                `(("type" . ,type)
                                  ("buttons" . ,buttons)
                                  ("default-id" . ,default-id)
                                  ("title" . ,title)
                                  ("message" . ,message)
                                  ("detail" . ,detail)
                                  ("cancel-id" . ,cancel-id)
                                  ("noLink" . ,no-link)))))
    (show-standard-dialog "showMessageBox" window options)))

(defun show-error-box (title content)
  "Display a dialog that shows an error message."
  (sync-js *driver*
           (format nil "return Ceramic.dialog.showErrorBox(~S, ~S);"
                   title content)))

