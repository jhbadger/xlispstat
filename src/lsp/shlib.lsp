(defpackage "SHARED-LIBRARY" (:use "XLISP") (:nicknames "SHLIB"))
(in-package "SHARED-LIBRARY")

;;;;
;;;; Data Structure for Library 
;;;;

(defstruct (shared-library
            (:constructor (make-shared-library (name path handle subrs)))
            (:print-function print-shlib))
  name path handle subrs)

(defun print-shlib (shlib stream depth)
  (format stream "#<shared library ~s>" (shared-library-name shlib)))


;;;;
;;;; Public Functions
;;;;

(export '(load-shared-library close-shared-library
          shared-library-information))

(defun load-shared-library (path &optional
                                 (name (pathname-name path))
                                 (version -1)
                                 (oldest version))
  (let ((*package* *package*)
        (handle (shlib-open path))
        (success nil))
    (unwind-protect
        (let* ((init (shlib-symaddr handle (format nil "~a__init" name)))
               (ftab (call-by-address init))
               (subrs (shlib-init ftab version oldest))
               (shlib (make-shared-library name path handle subrs)))
          ;;(register-saver shlib #'close-shared-library)
          (setf success t)
          shlib)
      (unless success (shlib-close handle)))))

(defun close-shared-library (shlib)
  ;;(unregister-saver shlib)
  (dolist (s (shared-library-subrs shlib))
    (clear-subr s))
  (shlib-close (shared-library-handle shlib)))

(defun shared-library-information (path &optional (name (pathname-name path)))
  (let ((*package* *package*)
        (handle (shlib-open path)))
    (unwind-protect
        (let* ((init (shlib-symaddr handle (format nil "~a__init" name)))
               (ftab (call-by-address init)))
          (shlib-info ftab))
      (shlib-close handle))))
