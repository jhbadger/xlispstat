;(expand 20)

(load "../cmplsp/common")
(load "../cmplsp/common2")
(load "../cmplsp/common3")
(load "../cmplsp/pathname")
(load "../cmplsp/loadfsl")
(load "../cmplsp/conditns")
(load "../cmplsp/shlib")

(push :packages *features*)
(push :times *features*)
(push :posfcns *features*)
(push :math *features*)
(push :mulvals *features*)
(push :bignums *features*)

(xlisp::use-conditions)

(defpackage "XLSCMP"
  (:use "XLISP")
  (:import-from "XLISP" "*CMP-SETF*" "*CMP-STRUCTS*" "*CMP-GLOBAL-MACROS*"
		"*CMP-MACROS*" "*CMP-SPECIALS*"))

(setf xlisp::*default-path* nil)

(cond
 ((probe-file "cmpfront.fsl") (load "cmpfront"))
 (t (load "../compiler/backquot")
    (load "../compiler/cmpmacro")
    (load "../compiler/convert")
    (load "../compiler/cells")
    (load "../compiler/simplify")
    (load "../compiler/lift")
    (load "../compiler/gencode")
    (load "../compiler/peephole")
    (load "../compiler/assemble")
    (load "../compiler/cmpfront")))

(in-package "XLISP")

(export '(compiler-let define-setf-method define-modify-macro locally))
(export '(save restore))
(export 'variable)

(defun initialize-system ()
  (setf *load-pathname-defaults*
	(list *default-path*
	      (merge-pathnames (make-pathname :directory
					      '(:relative "Autoload"))
			       *default-path*)))

  ;; load autoload information
  (setf *condition-hook* 'condition-hook)
  (mapc #'register-autoloads (create-autoload-path))

  ;; initialize module search path
  (setf *module-path* (create-module-path)))

(setf *startup-functions* '(use-conditions initialize-system))
