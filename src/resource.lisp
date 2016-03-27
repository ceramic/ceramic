(in-package :cl-user)
(defpackage ceramic.resource
  (:use :cl)
  (:import-from :ceramic.error
                :no-such-tag)
  (:import-from :ceramic.runtime
                :*releasep*
                :executable-relative-pathname)
  (:export :define-resources
           :resource-directory
           :resource
           :copy-resources)
  (:documentation "Ceramic's resource-management utilities."))
(in-package :ceramic.resource)

;;; Resources

(defclass resource ()
  ((tag :reader resource-tag
        :initarg :tag
        :type symbol
        :documentation "The resource tag.")
   (pathname :reader resource-pathname
             :initarg :pathname
             :type pathname
             :documentation "The directory, relative to the system directory.")
   (system :reader resource-system
           :initarg :system
           :type keyword
           :documentation "The name of the system this resource is relative to."))
  (:documentation "A resource."))

(defvar *resources* (list)
  "The list of resources.")

(defun get-resource (resource-tag)
  "Return the resource associated to a resource tag."
  (or (find resource-tag *resources* :key #'resource-tag :test #'eq)
      (error 'no-such-tag :tag resource-tag)))

;;; Define resources

(defun call-define-resources (system-name pairs)
  (loop for (tag . directory) in pairs do
    (push (make-instance 'resource
                         :tag tag
                         :pathname directory
                         :system system-name)
          *resources*)))

(defmacro define-resources (system-name () &rest pairs)
  "Define resource tags for a system."
  `(call-define-resources ,system-name
                          (list
                           ,@(loop for (tag source-directory) in pairs collecting
                              `(cons ',tag ,source-directory)))))

;;; Find resources

(defun release-resources-directory ()
  "The directory where resource directories are stored, relative to the
executable pathname."
  (executable-relative-pathname #p"resources/"))

(defun resource-directory (resource-tag)
  "Return the directory associated to a resource tag."
  (let ((resource (get-resource resource-tag)))
    (if *releasep*
        ;; Find the pathname relative to the executable pathname
        (merge-pathnames (resource-pathname resource)
                         (release-resources-directory))
        ;; Return the original pathname
        (asdf:system-relative-pathname (resource-system resource)
                                       (resource-pathname resource)))))

(defun resource (resource-tag pathname)
  "Return the pathname of a resource relative to the directory of a resource
tag."
  (merge-pathnames pathname (resource-directory resource-tag)))

(defun copy-resources (directory)
  "Copy all resource directories to @cl:param(directory)."
  (loop for resource in *resources* do
    (let* ((source-directory (asdf:system-relative-pathname (resource-system resource)
                                                            (resource-pathname resource)))
           (destination-name (string-downcase (symbol-name (resource-tag resource))))
           (destination (merge-pathnames
                         (make-pathname :directory (list :relative
                                                         destination-name))
                         directory)))
      (copy-directory:copy source-directory destination))))
