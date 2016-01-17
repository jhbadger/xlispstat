; initialization file for XLISP-STAT 2.1

(defun xlisp::small-machine-p () nil)
(export 'xlisp::small-machine-p 'xlisp)

;; get some more space
#+unix (expand 25)
#-unix (expand 15)

; disable strict keyword checking
(setf *strict-keywords* nil)

; restore old printing
;(setf *float-format* "%g")

; enable macro displacement and setf simplification (both a bit risky)
(setf xlisp::*displace-macros* t)
(setf xlisp::*simplify-setf* t)

; load in lisp files
(load "common")
(load "common2")
(load "common3")
(load "pathname")
(load "loadfsl")
(load "conditns")
(load "shlib")
#+unix (load "cmpload")

; initialize to disable breaks and trace back
;(setf *breakenable* t)
;(setf *tracenable* t)
;(setf *tracelimit* 1)
;(setf *baktrace-print-arguments* nil)
(setf *interrupt-action* #'top-level)
(setf *debug-print-length* 4)
(setf *debug-print-level* 3)

(setf xlisp::*default-handler*
      #'(lambda (c)
	  (when (and (null *breakenable*) (typep c 'error))
		(let ((*print-readably* nil))
		  (format *error-output* "~&Error: ~a~%" c)
		  (let ((f (xlisp::debug-fun)))
		    (if f (format *error-output* "Happened in: ~s~%" f))))
		(abort))))

; load xlispstat objects and related functions
(require "help")
(require "objects")
(require "menus")
#+msdos (require "dde")

; load statistics and graphics functions
(require "dialogs")
(require "linalg")
(require "stats")
(require "graphics")
(require "regress")

(in-package "XLISP")

(defvar *help-file-name*)
(defvar *help-stream*)
(defvar *line-length* 78 "Line length used in printing help messages")
(defvar *help-loaded* nil)

(defun initialize-system ()
  (setf *load-pathname-defaults*
	(list *default-path*
	      (merge-pathnames (make-pathname :directory
					      '(:relative "Autoload"))
			       *default-path*)))
  (setf *help-file-name* (merge-pathnames "xlisp.hlp" *default-path*))

  ;; adjust *features*
  (when (system-has-windows)
	(pushnew :windows *features*)
	(pushnew :dialogs *features*)
	(if (screen-has-color) (pushnew :color *features*))
	(if (small-machine-p) (pushnew :small-machine *features*)))

  (setf *help-stream* (open *help-file-name* :if-does-not-exist nil))
  ;;(setf *help-loaded* nil)

  ;; install macintosh listener and menu bar
  #+macintosh
  (progn
    (setq *listener* (send listener-proto :new))
    (set-menu-bar *standard-menu-bar*))

  ;; install MS Windows menu bar
  #+msdos
  (if (system-has-windows)
      (set-menu-bar *standard-menu-bar*))

  ;; load autoload information
  (setf *condition-hook* 'condition-hook)
  (mapc #'register-autoloads (create-autoload-path))

  ;; initialize module search path
  (setf *module-path* (create-module-path))

  ;; load user initialization file
  (let ((*package* (find-package "USER")))
    (load "statinit" :if-does-not-exist nil)))

(setf *startup-functions* '(use-conditions initialize-system))
