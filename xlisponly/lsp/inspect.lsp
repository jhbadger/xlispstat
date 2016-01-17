; XLISP INSPECTOR/EDITOR by TOM ALMY
; This is a rewritten and improved version of "REPAIR"
; Revised 10/96 to include functions INSPECT and DESCRIBE

; Use as (ins <symbol>)  or (insf <symbol>) to repair only the function
; binding, with the capability of changing the argument list and type
; (MACRO or LAMBDA). Functions INS and INSF are interned in package TOOLS.

; Common Lisp standard functions (inspect <object>) and (describe <object>)
; do not quote their arguements and work with any arbitrary object.

; Editor alters the "selection" by copying so that aborting  all changes
;  is generally posible.
; Exception: when editing a closure, if the closure is Backed out of, the
;   change is permanent.

; Do not create new closures, because the environment will be incorrect.

; Closures become LAMBDA or MACRO expressions when edited.  Only
; the closure body may be changed; the argument list cannot be successfully
; modified, nor can the environment.

; For all commands taking a numeric argument, the first element of the
; selection is the 0th (as in NTH function).

; Structure elements, class methods, instance variables, and properties
; are selected by name, using the E command.

; For class objects, only the methods, selectors and class variables
; can be edited. Class variables can only be changed if instance variables
; of class instances can be changed (see next paragraph).

; For instance objects, instance variables can be examine
; (if the object understands the message :<ivar> for the particular ivar),
; and changed if :SET-IVAR is defined for that class (as it is if CLASSES.LSP
; is used)

; COMMANDS (case is significant):
;	A -- select the CAR of the current selection.
;	D -- select the CDR of the current selection.
;	e n -- select element n
;	r n x -- replaces element n with (quoted) x.
;	X -- exit, saving all changes
;	Q -- exit, without changes
;	b -- go back one level (as before A, D or e commands)
;       B n -- go back n levels.
;	l -- display selection using pprint; if selection is symbol, give
;	     short description
;	v -- verbosity toggle
;       . n -- change maximum print length (default 10)
;       # n -- change maximum print depth (default 3)    
;	! x -- evaluates x and prints result
;		The symbol tools:@ is bound to the selection
;	R x -- replaces the selection with evaluated x.
;		The symbol tools:@ is bound to the selection
; additional commands if selection is a list or array:
;	( n m -- inserts parenthesis starting with the nth element,
;		for m elements.
;	) n -- removes parenthesis surrounding nth element of selection,
;              which may be array or list
;	[ n m -- as in (, but makes elements into an array
;	i n x -- inserts (quoted) x before nth element in selection.
;	d n -- deletes nth element in selection.
; additional command if selection is a list:
;	S x y -- all occurances of (quoted) y are replaced with 
;		(quoted) x.  EQUAL is used for the comparison.


#+:packages
(unless (find-package "TOOLS")
	(make-package "TOOLS" :use '("XLISP")))

(in-package "XLISP")
(export '(inspect describe)) ;; Common Lisp standard functions we'll define

(in-package "TOOLS")

(export '(ins insf @))

; Global variable used by inspect functions

(defparameter *ins-exit* 0)   ; "returning" flag
(defparameter *ins-name* nil) ; name of what we are editing

(defvar *ins-plev* 3)	; initial print level used
(defvar *ins-plen* 10)	; initial print length used
(defvar *verbosity* t)  ; printing verbosity flag					   
(defconstant *LPAR* #\()
(defconstant *RPAR* #\))

			     
; inspect a symbol -- the generic entry point

(defmacro ins (a)
    (unless (symbolp a) (error "~s is not a symbol" a))
    (let
     ((*breakenable* t)
      (*ins-exit* 0)
      (*ins-name* (cons "symbol" a))
      (*print-level* *ins-plev*)
      (*print-length* *ins-plen*))
     (catch 'abort (ins-ins a)))
     `',a)

; inspect a function, with editable arguments

(defmacro insf (a)
    (let
     ((*breakenable* nil)
      (*ins-exit* 0) 
      (*ins-name* (cons "function" a)) 
      (*print-level* *ins-plev*)
      (*print-length* *ins-plen*))
     (catch 'abort
	    (if (and (fboundp a) (typep (symbol-function a) 'closure))
		(let ((x (ins-ins(get-lambda-expression(symbol-function a)))))
		     (case (first x)
			   (lambda `(defun ,a ,@(rest x)))
			   (macro  `(defmacro ,a ,@(rest x)))
			   (t (error "not a closure!"))))
		(error "can't repair")))))

; Inspect anything

(defun xlisp::inspect (a)
       (let
	((*breakenable* t)
	 (*ins-exit* 0)
	 (*ins-name* (cons (if (symbolp a) "symbol" "expression") a))
	 (*print-level* *ins-plev*)
	 (*print-length* *ins-plen*))
	(catch 'abort (ins-ins a)))
       a)


; Describe anything

(defun xlisp::describe (a)
       (let ((*print-level* *ins-plev*)
	     (*print-length* *ins-plen*))
	    (format t "~&~s is " a)
	    (if	(or (typep a 'struct)
		    (objectp a))
		(ins-display a)
		(case (type-of a)
		      (list (format t "NIL"))
		      (cons (if (list-length a)
				(format t "a list of length ~s" (length a))
				(format t "a circular list")))
		      (array (format t "an array of length ~s" (length a)))
		      (string (format t "a string of length ~s" (length a)))
		      (hash-table (format t "a hash table with ~s entries"
					  (hash-table-count a)))
		      (symbol
		       (ins-display a)
		       (if (symbol-package a)
			   (format t "Home package: ~a~%" (package-name (symbol-package a)))
			   (format t "No home package~%"))
		       (format t "Visible in: ")
		       (map nil
			    (lambda (x)
				    (multiple-value-bind
				     (val type)
				     (find-symbol (symbol-name a) x)
				     (when (and type (eq val a))
					   (format t "~a (~a)  "
						   (package-name x)
						   (case type
							 (:external "ext")
							 (:internal "int")
							 (t "inh"))))))
			    (list-all-packages))
		       (terpri))
		      (t (format t "a ~(~a~)" (type-of a)))))))

; ins-propp returns T if p is a property of a

(defun ins-propp (a p)
	(do 	((plist (symbol-plist a) (cddr plist)))
		((or (null plist) (eq (car plist) p))
		 (not (null plist)))))

; terminate input line

(defun ins-teread (error) 
       (fresh-line)
       (if (not (eq (peek-char) #\Newline))
	   (read-line))
       (if error
	   (format t "Try again:")
	   (format t "~a ~a>" (car *ins-name*) (cdr *ins-name*))))

(defmacro ins-protread () ;;Protected read -- we handle errors
	  '(do ((val (errset (read)) 
		     (progn (ins-teread t) (errset (read)))))
	       ((consp val) (car val))))

(defmacro ins-proteval () ;;protected eval -- we handle errors
			  ;; we also use evalhook so environment is global
			  ;;  plus a local @, which cannot be changed!
	'(do* ((env (cons (list (list (cons '@ list))) nil))
	       (val (errset (evalhook (read) nil nil env))
		    (progn (ins-teread t) 
			   (errset (evalhook (read) nil nil env)))))
	      ((consp val) (car val))))


; New methods so that we can inspect and repair messages.

(send Class :answer :messages '() '(messages))

; new methods so that we can inspect and repair instance variables

(send Class :answer :ivars '() '(ivars))

(send Class :answer :cvars '() '((map 'list #'cons cvars cvals)))

(send Class :answer :superclass '() '(superclass))

#+:packages (import '(xlisp::%struct-ref xlisp::%struct-set))

(defun ins-struct (struct name)  ; get structure element
       (%struct-ref struct
		    (1+ (position name
				  (get (type-of struct) '*struct-slots*)
				  :key #'first))))

(defun ins-set-struct (struct name value)  ; set structure element
       (%struct-set struct
		    (1+ (position name
				  (get (type-of struct) '*struct-slots*)
				  :key #'first))
		    value))

#+:packages (unintern 'xlisp::%struct-ref)
#+:packages (unintern 'xlisp::%struct-set)


(defun ins-ivar (obj name)
       (funcall #'send obj
#+:packages	(intern (string name) :keyword)
#-:packages      (intern (strcat ":" name))
		))

(defun ins-set-ivar (obj name value)
       (funcall #'send obj :set-ivar
#+:packages	(intern (string name) :keyword)
#-:packages     (intern (strcat ":" name))
		value))

; help function
(defun ins-help (list)
       (format t "~%Available commands:~2%")
       (format t "e n~8tselect element n~%")
       (format t "r n x~8treplaces element n with (quoted) x.~%")
       (format t "X~8texit, saving all changes~%")
       (format t "Q~8texit, without changes~%")
       (format t "b~8tgo back one level (as before A, D or e commands)~%")
       (format t "B n~8tgo back n levels.~%")
       (format t (if (symbolp list)
		     "l~8tshow symbol~%"
		     "l~8tdisplay selection using pprint~%"))
       (format t "v~8tverbosity toggle~%")
       (format t ". n~8tchange maximum print length (default 10)~%")
       (format t "# n~8tchange maximum print depth (default 3)~%")
       (format t "! x~8tevaluates x and prints result.~%~8tThe symbol tools:@ is bound to the selection~%")
       (format t "R x~8treplaces the selection with evaluated x.~%~8tThe symbol tools:@ is bound to the selection~%")
       (unless (typep list '(or cons array)) (return-from ins-help list))
       (format t "A~8tselect the CAR of the current selection.~%")
       (format t "D~8tselect the CDR of the current selection.~%")
       (format t "( n m~8tinserts parens from nth element for m elements.~%")
       (format t ") n~8tremoves parens around nth element of selection.~%")
       (format t "[ n m~8tas in [, but makes elements into an array.~%")
       (format t "i n x~8tinserts (quoted) x before nth element in selection.~%")
       (format t "d n~8tdeletes nth element in selection.~%")
       (unless (typep list 'cons) (return-from ins-help list))
       (format t "S x y~8tall occurances of (quoted) y are replaced with~%~8t(quoted) x.  EQUAL is used for the comparison.~%")
       list)


; Display current selection
(defun ins-display (list)
       (fresh-line)
       (cond ((typep list '(or cons array))
	      (let ((n 0))
		   (if (or (arrayp list) (list-length list))
		       (map nil #'(lambda (l)
					  (format t "~3@s ~s~%" n l)
					  (setq n (1+ n)))
			    list)
		       (format t "circular list ~s~%" list))))
	     ((typep list 'struct)
	      (format t "~a structure ~%" (type-of list))
	      (mapc #'(lambda (n)
			      (format t
				      "~10s~s~%"
				      (car n)
				      (ins-struct list (car n))))
		    (get (type-of list) '*struct-slots*)))
	     ((classp list)
	      (format t "Class ~s, messages:~%" (send list :pname))
	      (mapc #'(lambda (n) (format t " ~s" (car n)))
		    (send list :messages))
	      (terpri)
	      (when (send list :cvars)
		    (format t "~%cvars:~%")
		    (mapc #'(lambda (n) (format t " ~s ~s~%" (car n) (cdr n)))
			  (send list :cvars))))
	     ((objectp list)
	      (format t "A ~s, ivars:~%" (send (send list :class) :pname))
	      (mapc #'(lambda (n) (format t " ~s ~s~%" n (ins-ivar list n)))
		    (send (send list :class) :ivars))
	      (terpri))
	     ((symbolp list)
	      (format t "Symbol ~s:~%" (symbol-name list))
	      (when (fboundp list) (format t "Function binding~%"))
	      (when (boundp list)
		    (format t "Value binding~a: ~s~%"
			    (cond ((constantp list) " (constant)")
				  ((specialp list) "  (special)")
				  (t ""))
			    (symbol-value list)))
	      (when (symbol-plist list)
		    (format t "Properties:~%")
		    (do ((l (symbol-plist list) (cddr l)))
			((null l) nil)
			(format t " ~s ~s~%" (first l) (second l)))))
	     (t (pprint list)))
       list)


; Bad command
(defun ins-bad (list)
       (format t "~&What??~%")
       list)
	  
;; Expects number >=min and <max (if max non-nil)
;; returns valid number, or prints message and returns nil
(defun ins-number (min max err &aux (n (ins-protread)))
       (if (and (numberp n)
		(>= n min)
		(or (null max) (< n max)))
	   n
	   (if err (ins-bad nil) nil)))

; inspect and replace list/array/structure/object/symbol elements
(defun ins-list-spec (list)
       (princ " element #? ")
       (ins-number 0 (length list) nil))

(defun ins-struct-spec (list &aux name)
       (princ " element name? ")
       (when (assoc (setq name (ins-protread))
		    (get (type-of list) '*struct-slots*))
	     name))

(defun ins-class-spec (list &aux name)
       (princ " message/cvar name? ")
       (cond ((assoc (setq name (ins-protread))
		     (send list :messages))
	      (cons t name))
	     ((assoc name (send list :cvars))
	      (cons nil name))))
       
(defun ins-object-spec (list &aux name)
       (princ " ivar name? ")
       (when (member (setq name (ins-protread))
		    (send (send list :class) :ivars))
	     name))

(defun ins-symbol-spec (list &aux name)
       (if (and (boundp list) (not (fboundp list)) (not (symbol-plist list)))
	   :v
	   (if (and (not (boundp list)) (fboundp list)
		    (not (symbol-plist list)))
	       :f
	       (progn
		(princ " :f :v or propname?")
		(if (ins-propp list (setq name (ins-protread)))
		    name
		    (case name ((:f :v) name)))))))

(defun ins-enter (list &aux val)
       (cond ((typep list '(or cons array))
	      (if (setq val (ins-list-spec list))
		  (concatenate (type-of list)
			       (subseq list 0 val)
			       (list (ins-ins (elt list val)))
			       (subseq list (1+ val)))
		  (ins-bad list)))
	     ((typep list 'struct)
	      (if (setq val (ins-struct-spec list))
		  (progn (ins-set-struct list val
					 (ins-ins (ins-struct list val)))
			 list)
		  (ins-bad list)))
	     ((classp list)
	      (if (setq val (ins-class-spec list))
		  (if (car val)
		      (let ((closure (cdr (assoc (cdr val)
						 (send list :messages))))
			    closure2 result)
			   (unless (typep closure 'closure)
				   (ins-bad list)
				   (return-from ins-enter list))
			   (setq closure2 (get-lambda-expression closure))
			   (setq result (ins-ins closure2))
			   (setf (cdr (cddr closure2)) (cdddr result))
			   (setf (car (cddr closure2)) (caddr result))
			   list)
		      (progn (ins-set-ivar (send list :new)
					   (cdr val)
					   (ins-ins (ins-ivar (send list :new)
							      (cdr val))))
			     list))
		  (ins-bad list)))
	     ((objectp list)
	      (if (setq val (ins-object-spec list))
		  (progn
		   (ins-set-ivar list val (ins-ins (ins-ivar list val)))
		   list)
		  (ins-bad list)))
	     ((typep list 'closure)
	      (let* ((x (get-lambda-expression list))
		     (y (ins-ins x)))
		    (setf (cdr (cddr x)) (cdddr y))
		    (setf (car (cddr x)) (caddr y))
		    list))
	     ((symbolp list)
	      (if (setq val (ins-symbol-spec list))
		  (case val
			(:f
			 (if (and (fboundp list)
				  (typep (symbol-function list) 'closure))
			     (let* ((x
				     (get-lambda-expression
				      (symbol-function list)))
				    (y (ins-ins x)))
				   (setf (cdr (cddr x)) (cdddr y))
				   (setf (car (cddr x)) (caddr y))
				   list)
			     (ins-bad list)))
			(:v
			 (if (boundp list)
			     (let* ((*ins-name* (cons "symbol" list))
				    (result (ins-ins (symbol-value list))))
				   (if (constantp list)
				       list
				       (progn (set list result) list)))
			     (ins-bad list)))
			(t (setf (get list val)
				 (ins-ins (get list val)))
			   list))))))
			      
(defun ins-repwith (list)
       (format t "~&Replace with: ")
       (ins-protread))

(defun ins-replace (list &aux val)
       (cond ((typep list '(or cons array))
	      (if (setq val (ins-list-spec list))
		  (concatenate (type-of list)
			       (subseq list 0 val)
			       (list (ins-repwith list))
			       (subseq list (1+ val)))
		  (ins-bad list)))
	     ((typep list 'struct)
	      (if (setq val (ins-struct-spec list))
		  (progn (ins-set-struct list val
					 (ins-repwith list))
			 list)
		  (ins-bad list)))
	     ((classp list)	; gotta catch this error here
	      (ins-bad list))
	     ((objectp list)
	      (if (setq val (ins-object-spec list))
		  (progn
		   (ins-set-ivar list val (ins-repwith list))
		   list)
		  (ins-bad list)))
	     ((symbolp list)
	      (if (setq val (ins-symbol-spec list))
		  (case val
			(:f
			 (ins-bad list))
			(:v
			 (if (not (constantp list))
			     (progn (setf (symbol-value list)
					  (ins-repwith list))
				    list)
			     (ins-bad list)))
			(t (setf (get list val)
				 (ins-repwith list))
			   list))))))

; main list repair interface
(defun ins-ins (list) 
       (ins-display list)
       (prog (command n newlist)
	     y (ins-teread nil)
	     (setq command (int-char (get-key)))  ;; Works with most systems
	     (princ command)
	     (setq
	      newlist  ;; new list value, if any
	      (case
	       command
	       (#\? (ins-help list))
	       (#\v (if (setq *verbosity* (not *verbosity*))
			(ins-display list)
			list))
	       (#\X (setq *ins-exit* -1) list)
	       (#\Q (throw 'abort))
	       (#\b (format t "ack") (return list))
	       (#\B (format t "ack #? ")
		    (when (setq n (ins-number 1 nil t))
			  (setq *ins-exit* n))
		    list)
	       (#\l (if *verbosity*
			(if (symbolp list)
			    (ins-display list)
			    (progn (terpri) (pprint list)))
			(format t "~%~s~%" list))
		    list)
	       (#\! (format t " Eval:~%")
		    (print (ins-proteval))
		    list)
	       (#\R (format t "eplace w. evaled:~%")
		    (ins-proteval))
	       (#\# (format t " print-level? ")
		    (when (setq n (ins-number 1 nil t))
			  (format t "Was ~s\n" *print-level*)
			  (setq *print-level* n))
		    list)
	       (#\. (format t " print-length? ")
		    (when (setq n (ins-number 1 nil t))
			  (format t "Was ~s\n" *print-length*)
			  (setq *print-length* n))
		    list)
; cons only commands
	       (#\A (if (consp list)
			(cons (ins-ins (car list)) (cdr list))
			(ins-bad list)))
	       (#\D (if (consp list)
			(cons (car list) (ins-ins (cdr list)))
			(ins-bad list)))
; various special commands
	       (#\e (if (typep list '(or cons struct array object symbol closure))
			(ins-enter list)
			(ins-bad list)))
	       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		(setq n (- (char-int command) (char-int #\0)))
		(if (and (typep list '(or cons array))
			 (< n (length list)))
		    (concatenate (type-of list)
				 (subseq list 0 n)
				 (list (ins-ins (elt list n)))
				 (subseq list (1+ n)))
		    (ins-bad list)))
	       (#\r (if (typep list '(or cons struct array object symbol))
			(ins-replace list)
			(ins-bad list)))
	       (#.*RPAR* (if (and (typep list '(or cons array))
			      (princ " remove nesting at #? ")
			      (setq n (ins-number 0 (length list) nil))
			      (typep (elt list n) '(or cons array)))
			 (concatenate (type-of list)
				      (subseq list 0 n)
				      (elt list n)
				      (subseq list (1+ n)))
			 (ins-bad list)))
	       ((#.*LPAR* #\[)
		(if (and (typep list '(or cons array))
			 (princ " insert nesting starting at # and length? ")
			 (setq n (ins-number 0 nil nil))
			 (setq n2 (ins-number 1
					      (- (1+ (length list)) n)
					      nil)))
		    (concatenate (type-of list)
				 (subseq list 0 n)
				 (list (coerce (subseq list n (+ n n2))
					       (if (eq command *LPAR*)
						   'list
						   'array)))
				 (subseq list (+ n n2)))
		    (ins-bad list)))
	       (#\i (if (and (typep list '(or array cons))
			     (princ "nsert before # and value? ")
			     (setq n (ins-number 0 (1+ (length list)) nil)))
			(concatenate (type-of list)
				     (subseq list 0 n)
				     (list (ins-protread))
				     (subseq list n))
			(ins-bad list)))
	       (#\d (if (and (typep list '(or array cons))
			     (princ "elete #? ")
			     (setq n (ins-number 0 (length list) nil)))
			(concatenate (type-of list)
				     (subseq list 0 n)
				     (subseq list (1+ n)))
			(ins-bad list)))
	       (#\S (if (typep list 'cons)
			(progn
			 (princ "ubstitute expr with expr\n")
			 (subst (ins-protread) 
				(ins-protread) 
				list
				:test #'equal))
			(ins-bad list)))
		   (#\Newline list)
		   (t (ins-bad list))))
	     (when (not (eq list newlist))	    ;; show any changes
		   (setq list newlist)
		   (when (and (zerop *ins-exit*) *verbosity*)
			 (ins-display list)))
	     (when (zerop *ins-exit*) (go y))
	     (setq *ins-exit* (1- *ins-exit*)) ;; return a level
	     (return list)))
