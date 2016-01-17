;;;; This file provide various documentation functions, taking some existing
;;;; XLISP-PLUS functionality and merging it with new Common Lisp functions.

; Author -- Tom Almy,  10/96

;;; It supercedes glos.lsp.  Do not load glos.lsp if this file is used!

;;; This file adds these new functions:

;;; DOCUMENTATION -- get/set documentation string.
;;; This function will fetch glossary info
;;; for functions and variables defined in glos.txt (see description of GLOS
;;; below) as well provided via DOCUMENTATION used with SETF or with
;;; the DEF* macros listed below.

;;; The documentation types variable, function, structure, and setf are stored
;;; in properties named %doc-function, %doc-structure, %doc-variable, and
;;; %doc-setf. In addition, documentation type type with property %doc-type
;;; is provided for completeness.

;;; DEFCONSTANT, DEFPARAMETER, DEFVAR, DEFUN, DEFMACRO, DEFSTRUCT, and DEFSETF
;;; are modified to have the documentation arguments functional.

;;; GLOS -- glossary function

; GLOS requires the package and multiple value return facilities to
; work, and uses a file called glos.txt which is the glossary portion
; of the XLISP documentation file When loaded for the first time, it
; adds documentation marks for all functions which are defined in
; glos.txt and are in the XLISP package.  This property is the
; displacement into the file. When a glossary lookup occurs (or the
; DOCUMENTATION function is used) the file itself is referenced. By
; operating this way, very little space is taken for this feature.

; There are two user-accessable symbols. tools:*glospaging* is a variable
; which causes the output to "page" (pause for user response) at every
; screenful. Set it to NIL to defeat this feature or to the number of lines
; per page to enable.

; The main entry point is the function tools:glos. When given an
; argument that is a function symbol, it will look up the glossary
; definition. If the symbol is not visible, or if a second non-nil
; argument is supplied, the name will be passed to APROPOS, and the
; glossary definitions for all matching symbols will be displayed

; For instance (glos :car) or (glos 'car) will show the definition for
; the CAR function, while (glos 'car t) or (glos "car") will show that
; of MAPCAR as well. (glos "X") will give the glossary listing of all
; functions with names containing an X character.


#-:packages
(error "This utility was written asuming the package facility is in use")

#-:mulvals
(error "This utility was written asuming multiple value return is in use")

#-:common
(load "common")       ;; make sure these are defined first

(in-package "XLISP")

(export '(documentation variable function structure setf))

(in-package "TOOLS")

		  
; This is the glos.lsp package, modified to be integrated with 
; DOCUMENTATION. Glos.lsp should not be loaded if this file is used!
;

(export '(glos *glospaging*))

(import '(xlisp::%doc-function xlisp::%doc-variable))

(defvar *glosfilename*)

(setq *glosfilename* nil)

; We will look things up while loading
; so we can toss all the code when done

(unless *glosfilename*
	(format t "~&Building glossary references---")
	(let ((lpar #\()
	      (rpar #\))
	      (dot #\.)
	      (*pos* 0)
	      symbol)
	     (labels
(
	      

(xposition (chr str &aux (pos (position chr str)))
       (if pos pos (length str)))

(seek-next-fcn (strm)
       (do ((thispos *pos* (file-position strm))
	    (text (read-line strm nil) (read-line strm nil)))
	   ((null text) nil)
	   (when (and (> (length text) 1)
		      (or (char= lpar (char text 0))
			  (char= dot (char text 0))))
		 (setf *pos* thispos)
		 (return-from seek-next-fcn
			      (cons (char= dot (char text 0))
				    (subseq text 1 (min (xposition rpar text)
						  (xposition #\space text)))))))))

;; The body of the code that does the work:		   
	      (unless (open "glos.txt" :direction :probe)
		      (error "Could not find glossary file glos.txt"))
	      (with-open-file
	       (strm "glos.txt")
	       (setq *glosfilename* (truename strm))
	       (do ((name (seek-next-fcn strm) (seek-next-fcn strm)))
		   ((null name) nil)
		   (setq symbol (find-symbol (string-upcase (cdr name))))
		   (unless symbol
			   (if (string-equal (cdr name) "nil")
			       (setf (get nil '%doc-variable) (abs *pos*))
			       (format t
				       "~&Documented symbol ~s not found in XLISP.~%"
				       (cdr name))))
		   (when symbol
;;			 (format t "~s ~s" symbol *pos*)
			 (setf (get symbol (if (car name)
					       '%doc-variable
					       '%doc-function))
			       (abs *pos*)))))
;; Check for functions & vars in package XLISP that aren't documented
	      (format t "~&Not documented, but found in XLISP:")
	      (do-external-symbols
	       (x :xlisp)
	       (when (or (and (fboundp x) (not (get x '%doc-function)))
			 (and (specialp x) (not (get x '%doc-variable))))
		     (format t "~s " x)))
	      (format t "~&")

))) ;; Ends the Flet, let, and unless

(defvar *linecount*)
(defvar *glospaging* 23)

(defun linechk ()
       (when (and *glospaging*
		  (> (incf *linecount*) *glospaging*))
	     (setq *linecount* 0)
	     (if (y-or-n-p "--PAUSED--  Continue?")
		 (fresh-line)
		 (throw 'getoutahere))))

(defun ppstring (string &aux (strm (make-string-input-stream string)))
       (do ((line (read-line strm nil) (read-line strm nil)))
	   ((zerop (length line))
	    (linechk)
	    (format t "~%"))
	   (linechk)
	   (format t "~a~%" line)))

(defun glosx (val &aux (ostrm (make-string-output-stream)))
       (with-open-file
	(strm *glosfilename*)
	(file-position strm (abs val))
	(do ((line (read-line strm nil) (read-line strm nil)))
	    ((zerop (length line))
	     (format ostrm "~%"))
	    (if (eq #\. (char line 0))
		(format ostrm "~a~%" (subseq line 1))
		(format ostrm "~a~%" line))))
       (get-output-stream-string ostrm))

(defun glos (symbol &optional matchall
		    &aux val val2 val3 (sym (string symbol)))
       (catch
	'getoutahere
	(setq *linecount* 0)
	(if (and (null matchall) (setq val (find-symbol sym)))
	    (progn (when (setq val2 (documentation val 'function))
			 (ppstring val2))
		   (when (setq val3 (documentation val 'variable))
			 (ppstring val3))
		   (unless (or val2 val3)
			   (format t "No information on ~a~%" sym)))
	    (progn
	     (setq val
		   (do ((list (apropos-list sym) (cdr list))
			(result nil result))
		       ((null list) result)
		       (when (setq val2 (documentation (car list) 'function))
			     (when (not (member val2 result :test #'string-equal))
				   (push val2 result)))
		       (when (setq val2 (documentation (car list) 'variable))
			     (when (not (member val2 result :test #'string-equal))
				   (push val2 result)))))
		       
	     (if (zerop (length val))
		 (format t "No matches for ~a~%" symbol)
		 (map nil #'ppstring val)))))
       (values)
)

(in-package "XLISP")
(defun documentation (sym type &aux value)
       (unless (symbolp sym) (error "bad argument type - ~s" sym))
       (setq
	value
	(case type
	      (variable (get sym '%doc-variable))
	      (function (get sym '%doc-function))
	      (structure (get sym '%doc-structure))
	      (setf (get sym '%doc-setf))
	      (type (get syp '%doc-type))
	      (t (error "invalid documentation type - ~s" type))))
       (if (numberp value)
	   (tools::glosx value)
	   value))

(defsetf documentation (sym type) (val)
	 (case (eval type)
	       (variable `(setf (get ,sym '%doc-variable) ,val))
       	       (function `(setf (get ,sym '%doc-function) ,val))
	       (structure `(setf (get ,sym '%doc-structure) ,val))
       	       (setf `(setf (get ,sym '%doc-setf) ,val))
       	       (type `(type (get ,sym '%doc-type) ,val))
	       (t (error "invalid documentation type - ~s" type))))

;; If we haven't done it before, save function binding of defining words

(unless (fboundp 'old-defun)
	(setf (symbol-function 'old-defun)
	      (symbol-function 'defun)))
(unless (fboundp 'old-defmacro)
	(setf (symbol-function 'old-defmacro)
	      (symbol-function 'defmacro)))
(unless (fboundp 'old-defvar)
	(setf (symbol-function 'old-defvar)
	      (symbol-function 'defvar)))
(unless (fboundp 'old-defparameter)
	(setf (symbol-function 'old-defparameter)
	      (symbol-function 'defparameter)))
(unless (fboundp 'old-defconstant)
	(setf (symbol-function 'old-defconstant)
	      (symbol-function 'defconstant)))
(unless (fboundp 'old-defstruct)
	(setf (symbol-function 'old-defstruct)
	      (symbol-function 'defstruct)))

;; Redefine defun, defmacro, defvar, defconstant, defparameter, and defstruct
;; to update the property list and then do the original function.
;; In the case of defmacro and defvar, the documentation string is removed
;; from the definition.

(defmacro defun (&rest arglist)
	  (if (and (stringp (third arglist)) (cdddr arglist))
	      (progn
	       (unless (symbolp (first arglist))
		       (error "bad argument type - ~s" (first arglist)))
	       (setf (get (first arglist) '%doc-function) (third arglist))
	       `(old-defun ,(first arglist)
			   ,(second arglist)
			   ,@(cdddr arglist)))
	      `(old-defun ,@arglist)))

(defmacro defmacro (&rest arglist)
	  (if (and (stringp (third arglist)) (cdddr arglist))
	      (progn
	       (unless (symbolp (first arglist))
		       (error "bad argument type - ~s" (first arglist)))
	       (setf (get (first arglist) '%doc-function) (third arglist))
	       `(old-defmacro ,(first arglist)
			   ,(second arglist)
			   ,@(cdddr arglist)))
	      `(old-defmacro ,@arglist)))

(defmacro defvar (&rest arglist)
	  (when (stringp (third arglist))
		(progn
		 (unless (symbolp (first arglist))
			 (error "bad argument type - ~s" (first arglist)))
		 (setf (get (first arglist) '%doc-variable) (third arglist))))
	  `(old-defvar ,@arglist))


(defmacro defparameter (&rest arglist)
	  (when (stringp (third arglist))
		(progn
		 (unless (symbolp (first arglist))
			 (error "bad argument type - ~s" (first arglist)))
		 (setf (get (first arglist) '%doc-variable) (third arglist))))
	  `(old-defparameter ,@arglist))


(defmacro defconstant (&rest arglist)
	  (when (stringp (third arglist))
		(progn
		 (unless (symbolp (first arglist))
			 (error "bad argument type - ~s" (first arglist)))
		 (setf (get (first arglist) '%doc-variable) (third arglist))))
	  `(old-defconstant ,@arglist))

(defmacro defstruct (&rest arglist)
	  (when (stringp (second arglist))
		(let ((sym (if (consp (first arglist))
			       (caar arglist)
			       (first arglist))))
		     (unless (symbolp sym)
			     (error "bad argument type - ~s" (first arglist)))
		     (setf (get sym '%doc-structure) (second arglist))))
	  `(old-defstruct ,@arglist))

