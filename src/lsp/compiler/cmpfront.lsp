(in-package "XLISP")

(export '(compile compile-file *compile-verbose* *compile-print*))

(defpackage "XLSCMP"
  (:use "XLISP")
  (:import-from "XLISP" "*CMP-SETF*" "*CMP-STRUCTS*" "*CMP-GLOBAL-MACROS*"
		"*CMP-MACROS*" "*CMP-SPECIALS*"	#+xlisp-stat "ADD-METHOD"))

(in-package "XLSCMP")

#|
put in package stuff
move macros to cmpmacr.lsp
move cps stuff to convert.lsp
|#

(require "backquot")

(require "cmpmacro")
(require "convert")
(require "cells")
(require "simplify")
(require "lift")
(require "gencode")
(require "peephole")
(require "assemble")

#|
Macros currently defined (**** need to be checked over ***):

DEFSETF PUSH PUSHNEW STEP WITH-INPUT-FROM-STRING WITH-OPEN-FILE
WITH-OUTPUT-TO-STRING


Macros needed (check which are really needed):

assert call-method ccase check-type ctypecase decf declaim defclass
defgeneric define-compiler-macro define-condition define-declaration
define-method-combination define-modify-macro define-setf-method
defmethod defpackage deftype destructuring-bind do-all-symbols
do-external-symbols do-symbols ecase encapsulated etypecase formatter
gathering generic-function handler-bind handler-case ignore-errors
in-package incf iterate loop-finish mapping multiple-value-bind
multiple-value-list multiple-value-setq next-in nth-value pop
pprint-exit-if-exhausted pprint-logical-block pprint-pop
print-unreadable-object producing psetf remf restart-bind restart-case
rotatef shiftf terminate-producing typecase with-accessors
with-compilation-unit with-condition-restarts with-hash-table-iterator
with-open-stream with-package-iterator with-simple-restarts with-slots
with-standard-io-syntax
|#

(defun pscvt (e)
  (progv '(*cmp-env*   *cmp-fenv*  *cmp-denv*   *cmp-tenv* *cmp-specials*
	   *cmp-gvars* *cmp-gfuns* *cmp-consts*
	   *cmp-specials* *cmp-macros* *cmp-setf* *cmp-structs*)
	 '(nil nil nil nil nil nil nil nil nil nil nil nil)
	 (let ((n (if (and (consp e) (eq (first e) 'lambda))
		      (convert-lambda e)
		      (convert-lambda `(lambda () ,e)))))
	   (pp-cps (merge-tests (simplify-tree (insert-cells n)))))))

		     
;;;;
;;;; Simple compiler front end
;;;;

(defun cmp (e &optional name)
  (progv '(*cmp-env* *cmp-denv* *cmp-tenv*
	   *cmp-gvars* *cmp-gfuns* *cmp-consts*)
	 '(nil nil nil nil nil nil)
	 (let ((n (if (and (consp e) (eq (first e) 'lambda))
		      (if name
			  (convert-named-lambda (cons name (rest e)))
			  (convert-lambda e))
		      (convert-lambda `(lambda () ,e)))))
	   (insert-cells n)
	   (substitute-all-variables n)
	   (collapse-null-lambda-calls n)
	   (simplify-tree n)
	   (merge-tests n)
	   (let ((pieces (lift-lambdas n)))
	     (dolist (p pieces)
	       (let ((c (second p)))
		 (extract-constants c)
		 (remove-unused-cells c)
		 ;;(mapcar #'pp-cps p)
		 ))
	     (generate-code pieces)))))

(defun pcmp (e) (pprint (cmp e)))

(defun cmp-reset ()
  (mapcar #'set
	  '(*cmp-env* *cmp-fenv* *cmp-denv* *cmp-tenv*
	    *cmp-specials* *cmp-macros* *cmp-setf* *cmp-structs*
	    *cmp-gvars* *cmp-gfuns* *cmp-consts*)
	  '(nil nil nil nil nil nil nil nil nil nil nil)))


(defparameter *cmp-files*
  '("backquot" "cmpmacro" "convert" "cells" "simplify"
    "lift" "gencode" "peephole" "assemble" "cmpfront"))

(defun compile-file-list (list &optional load)
  (progv '(*features*)
         #+unix '((WINDOWS DIALOGS COLOR UNIX X11 XLISP))
         #+macintosh '((WINDOWS DIALOGS MACINTOSH XLISP))
	 (dolist (f list)
	   (format t "~%Compiling file ~s ... ~%" f)
	   (compile-file f)
	   (format t "finished compiling file ~s~%~%" f)
	   (if load (load f)))))

(defun compile-cmp () (compile-file-list *cmp-files* t))

;;;;;
;;;;;
;;;;; New File Compiler
;;;;;
;;;;;

(defun print-compiled-form (form stream sp)
  (let ((*print-readably* t)
	(*print-symbol-package* sp))
    (format stream "~s~%" form)))

(defun accumulate-compiled-form (form outstuff)
  (rplacd (last outstuff) (list form)))

;;**** change default later?
(defvar *compile-verbose* t)
(defvar *compile-print* t)

(defvar *compile-print-symbol-package* nil)

(defvar *compile-warn-specials* nil)

(defvar *cmp-specials* nil)
(defvar *cmp-macros* nil)
(defvar *cmp-setf* nil)
(defvar *cmp-structs* nil)

#|
(defun file-name-needs-extension (fname)
  (not (eql #\.
	    (find-if-not #'(lambda (x) (or (digit-char-p x) (alpha-char-p x)))
			 fname
			 :from-end t))))
				 
(defun lsp-file-name (f)
  (if (file-name-needs-extension f)
      (concatenate 'string f ".lsp")
      f))

(defun fsl-file-name (f)
  (let ((n (length f)))
    (unless (and (<= 4 (length f)) (string= (subseq f (- n 4) n) ".lsp"))
	    (error "not a valid lisp file name -- ~s" f))
    (concatenate 'string (subseq f 0 (- n 4)) ".fsl")))
|#

(defun cmp-print-start-message (form)
  (when *compile-print*
	(let ((*print-level* 2)
	      (*print-length* 3))
	  (format t "~&; compiling ~s ... " form)
	  (force-output))))

(defun cmp-print-end-message ()
  (when *compile-print*
	(format t "done~%")
	(force-output)))

(defun expand-one (e)
  (if (consp e)
      (loop
       (if (member (first e) '(progn macrolet eval-when)) (return e))
       (multiple-value-bind (ee flag) (cmp-macroexpand-1 e)
         (if flag
	     (setq e ee)
	     (return e))))))

;;**** %set-cmp-macro???
(defun compile-one (e compile-time-too stream)
  (handler-case
   (progn
     (setq e (expand-one e))
     (case (first e)
       (progn (dolist (e (rest e)) (compile-one e compile-time-too stream)))
       (macrolet
	(let ((macs (second e))
	      (env (list nil *cmp-fenv* *cmp-macros* *cmp-global-macros*))
	      (frame nil)
	      (body (rest (rest e)))
	      (*cmp-fenv* *cmp-fenv*))
	  (dolist (m macs)
	    (push
	     (cons (first m)
		   (coerce-to-macro
		    (parse-macro (first m) (second m) (rest (rest m)) env)))
	     frame))
	  (dolist (x frame) (push x *cmp-fenv*))
	  (compile-one `(progn ,@body) compile-time-too stream)))
       (eval-when
	(let* ((sits (second e))
	       (body `(progn ,@(rest (rest e))))
	       (lt (or (member 'load sits) (member :load-toplevel sits)))
	       (ct (or (member 'compile sits) (member :compile-toplevel sits)))
	       (ex (or (member 'eval sits) (member :execute sits))))
	  (cond
	   ((or (and lt ct)
		(and lt (not ct) ex compile-time-too))
	    (compile-one body t stream))
	   ((or (and lt (not ct) (not compile-time-too))
		(and lt (not ct) (not ex)))
	    (compile-one body nil stream))
	   ((or (and (not lt) ct)
		(and (not lt) (not ct) ex compile-time-too))
	    (eval body)))))
       (t (if compile-time-too (eval e))
	  (compile-form e stream))))
   (error (c)
	  (format *error-output* "~&Compiler error: ~a~%" c)
	  ;;****(format stream "~&;;**** Error compiling exression:~%")
	  (compile-form `(eval ',e) stream))))

;;**** could ignore top level atoms, (function ...) expressions
(defun compile-form (e outstuff)
  (labels ((cmparg (e)
	     (cond
	      ((and (consp e)
		    (eq (first e) 'function)
		    (consp (second e))
		    (eq (first (second e)) 'lambda))
	       ;;**** should be put in quote?
	       `(byte-code-close (quote ,(assemble (cmp (second e))))))
	      ((consp e) (rcmp e))
	      (t e)))
	   (rcmp (e)
	     (if (consp e)
		 (let ((f (first e)))
		   (if (and (symbolp f)
			    (not (assoc f *cmp-fenv*))
			    (not (assoc f *cmp-macros*))
			    (not (assoc f *cmp-global-macros*))
			    (fboundp f)
			    (functionp (symbol-function f)))
		       `(,(first e) ,@(mapcar #'cmparg (rest e)))
		       e))
	         e)))
    (handler-case
     (let ((ce (rcmp e)))
       ;;**** don't need the test -- top level (function ...) 's are not done
       (accumulate-compiled-form
	(if (typep ce 'byte-code) ce (assemble (cmp ce)))
	;(if (typep ce 'byte-code) ce (assemble (cmp `(eval ',ce))))
	outstuff))
     (error (c)
	    (format *error-output* "~&Compiler error: ~a~%" c)
	    (accumulate-compiled-form (assemble (cmp `(eval ',e)))
				      outstuff)))))


#|
(defun compile-form (e outstuff)
    (accumulate-compiled-form (assemble (cmp e)) outstuff))
|#

(defun print-fsl-version-check (out sp)
  (let ((major xlisp::*fsl-major-version*)
	(minor xlisp::*fsl-minor-version*))
    (print-compiled-form `(xlisp::check-fsl-version ,major ,minor) out sp)))

;;**** need to avoid creating bad .fsl file
(defun compile-file (file &key
			  (output-file (merge-pathnames ".fsl" file))
			  (temporary-file (merge-pathnames "cmptmp.fsl" file))
			  (load nil)
			  ((:print *compile-print*) *compile-print*)
			  ((:verbose *compile-verbose*) *compile-verbose*)
			  ((:print-symbol-package sp) ;;**** do this cleaner
			   *compile-print-symbol-package*))
  (let* ((*package* *package*)
	 (*readtable* *readtable*)
	 (*cmp-specials* nil)
	 (*cmp-macros* nil)
	 (*cmp-setf* nil)
	 (*cmp-structs* nil)
	 (iname (merge-pathnames ".lsp" file)))
    (with-open-file (in iname)
      (with-open-file (out temporary-file :direction :output)
        (when *compile-verbose*
	      (format t "~&; compiling file ~s~%" iname)
	      (force-output))
	(print-fsl-version-check out sp)
        (let ((eof (list 'eof)))
	  (do ((e (read in nil eof) (read in nil eof)))
	      ((eq e eof))
	      (let ((outstuff (list 'progn)))
		(cmp-print-start-message e)
		(let ((*cmp-fenv* nil))
		  (compile-one e nil outstuff))
		(cmp-print-end-message)
		;; this insures that a single expression is printed for
		;; each expression read -- so common literals can be
		;; handled by circle printing/reading.
		(case (length outstuff)
		  (1 nil)
		  (2 (print-compiled-form (second outstuff) out sp))
		  (t (print-compiled-form (assemble (cmp `(eval ',outstuff)))
					  out
					  sp))))))))
    (rename-file temporary-file output-file)
    (if load (load output-file))))

;;**** this doesn't currently work for compiling macros
(defun compile-lambda-expression (fun name env macrop)
  (when env (error "COMPILE can only compile top level definitions"))
  (let ((*cmp-specials* nil)
	(*cmp-macros* nil)
	(*cmp-fenv* nil)
	(*cmp-setf* nil)
	(*cmp-structs* nil))
    (let* ((fun (if macrop (cons 'lambda (rest fun)) fun))
	   (cfun (byte-code-close (assemble (cmp fun name)))))
      (if macrop (coerce-to-macro cfun) cfun))))

(defun compile-return (f cfun)
  (if f
      (xlisp::install-function f cfun)
    cfun))
  
(defun compile (f &optional (fun (symbol-function f)))
  (cond
   ((or (typep fun 'subr) (typep fun 'fsubr) (compiled-function-p fun))
    (compile-return f fun))
   ((typep fun 'closure)
    (multiple-value-bind (flam top fname) (function-lambda-expression fun)
      (let ((name (if f f fname))
	    (macrop (eq (first flam) 'macro)))
	(compile-return f (compile-lambda-expression flam name top macrop)))))
   ((and (consp fun) (eq (first fun) 'lambda))
    (compile-return f (compile-lambda-expression fun f nil nil)))
   (t (error "bad argument type -- ~s" fun))))

(provide "cmpload")
