;;
;; Some compiler/loader support
;;

(in-package "XLISP")

;; needed by the compiler -- must appear before any defmeth's
#+xlisp-stat
(defun add-method (object name method doc)
  (if doc (send object :internal-doc name doc))
  (send object :add-method name method))

;; signal errors for old compiled defstructs
(defun cmp-make-structure-constructor (structname)
  (declare (ignore structname))
  (error "obsolete defstruct code -- file needs to be recompiled"))

(defun cmp-do-defstruct (structname incopt others slotargs)
  (declare (ignore structname incopt others slotargs))
  (error "obsolete defstruct code -- file needs to be recompiled"))

;; check fsl version
(defconstant *fsl-major-version* 1)
(defconstant *fsl-minor-version* 4)
(defconstant *fsl-oldest-minor-version* 3)

(defun check-fsl-version (major minor)
  (unless (and (= major *fsl-major-version*)
	       (<= *fsl-oldest-minor-version* minor *fsl-minor-version*))
	  (error "file FSL version ~d.~d is not compatible with ~
                  system version ~d.~d"
		 major minor
		 *fsl-major-version* *fsl-minor-version*)))
