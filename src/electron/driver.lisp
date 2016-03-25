(in-package :cl-user)
(defpackage ceramic.electron
  (:use :cl)
  (:import-from :ceramic.file
                :*ceramic-directory*)
  (:import-from :ceramic.os
                :*operating-system*
                :*architecture*)
  (:import-from :electron-tools
                :binary-pathname
                :get-release)
  ;; Functions
  (:export :*binary-pathname*
           :start-process
           :send-command)
  ;; Commands
  (:export :create-window
           :close-window
           :destroy-window
           :send-message
           :show-window
           :hide-window
           :resize-window
           :focus-window
           :maximize-window
           :unmaximize-window
           :minimize-window
           :unminimize-window
           :fullscreen-window
           :unfullscreen-window
           :resizable-window
           :unresizable-window
           :center-window
           :set-window-position
           :set-window-title
           :window-load-url
           :window-reload
           :window-open-dev-tools
           :window-close-dev-tools
           :window-undo
           :window-redo
           :window-cut
           :window-paste
           :window-delete
           :window-select-all
           :quit))
(in-package :ceramic.electron)
