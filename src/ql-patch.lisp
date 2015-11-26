;;;; Okay, so. So libraries use the sharpsign plus/minus macros to determine
;;;; whether Quicklisp is in the environment, and decide whether to use
;;;; `ql:quickload` or `asdf:load-system` to load a library. Libraries are
;;;; compiled with :quicklisp in *features* into fasl files. Then, when
;;;; compiling a Ceramic app, it loads those.
;;;;
;;;; Then it fails with "package `quicklisp-client` not found". Why can't we
;;;; make a list of those libraries and recompile them? Well, because that's
;;;; horrible, and also recompiling some libraries (looking at you,
;;;; clack-v1-compat) causes compilation errors due to package redefinition. So
;;;; what we do here is, we define the Quicklisp package and `quickload` to do
;;;; nothing. Why not make it call `asdf:load-system`? Because there's no ASDF
;;;; in a compiled application.

(defpackage quicklisp-client
  (:use :cl)
  (:nicknames :ql)
  (:export :quickload)
  (:documentation "Quicklisp look-alike fake package."))
(in-package :quicklisp-client)

(defun quickload (&rest args)
  "This is a load-bearing hack."
  t)
