; New Structure Editor (inspector)  by Tom Almy

; With advent of packages, this editor has been changed so that keywords
; are used for all commands. Special code will convert symbols (in the
; current package) accidentally used as commands into keywords!

; (repair <symbol>)  or (repairf <symbol>) to repair only the function
; binding, with the capability of changing the argument list and type
; (MACRO or LAMBDA).

; Editor alters the "selection" by copying so that aborting  all changes
;  is generally posible.
; Exception: when editing a closure, if the closure is BACKed out of, the
;   change is permanent.
; For all commands taking a numeric argument, the first element of the
; selection is the 0th (as in NTH function).

; Any array elements become lists when they are selected, and
; return to arrays upon RETURN or BACK commands.

; Do not create new closures, because the environment will be incorrect.

; Closures become LAMBDA or MACRO expressions when selected.  Only
; the closure body may be changed; the argument list cannot be successfully
; modified, nor can the environment.

; For class objects, only the methods and selectors can be modified.  For
; instance objects, instance variables can be examined (if the object under-
; stands the message :<ivar> for the particular ivar), and changed 
; if :SET-IVAR is defined for that class (as it is if CLASSES.LSP is used)

; Structures are now handled -- editing a structure will create an association
; list of the structure's elements. Returning will cause assignments to
; be made for all matching elements.


; COMMANDS:
;	:CAR -- select the CAR of the current selection.
;	:CDR -- select the CDR of the current selection.
;	n -- where n is small non-negative integer, changes selection
;			to (NTH n list)
;	:RETURN -- exit, saving all changes
;	:ABORT -- exit, without changes
;	:BACK -- go back one level (as before CAR CDR or N commands)
;       :B n -- go back n levels.
;	:L -- display selection using pprint; if selection is symbol, give
;	     short description
;	:MAP -- pprints each element of selection, if selection is symbol
;	          then give complete description of properties.
;       :PLEN n -- change maximum print length (default 10)
;       :PLEV n -- change maximum print depth (default 3)    
;	:EVAL x -- evaluates x and prints result
;		The symbol tools:@ is bound to the selection
;	:REPLACE x -- replaces the selection with evaluated x.
;		The symbol tools:@ is bound to the selection
; additional commands if selection is a symbol:
;	:VALUE -- edit value binding
;	:FUNCTION -- edit function binding (if a closure)
;	:PROP x -- edit property x
; additional commands if selection is a list:
;	:SUBST x y -- all occurances of (quoted) y are replaced with 
;		(quoted) x.  EQUAL is used for the comparison.
;	:RAISE n -- removes parenthesis surrounding nth element of selection
;	:LOWER n m -- inserts parenthesis starting with the nth element,
;		for m elements.
;	:ARRAY n m -- as in LOWER, but makes elements into an array
;	:I n x -- inserts (quoted) x before nth element in selection.
;	:R n x -- replaces nth element in selection with (quoted) x.
;	:D n -- deletes nth element in selection.

#+:packages
(unless (find-package "TOOLS")
	(make-package "TOOLS" :use '("XLISP")))

(in-package "TOOLS")

(export '(repair repairf @))

; Global variable used by repair functions
; Assuming globals are specials -- if you are using this with old XLISP
; then search for binding of globals, and change LET's to PROGV's

(defparameter *rep-exit* 0)   ; "returning" flag
(defparameter *rep-name* nil) ; name of what we are editing

(defvar *rep-plev* 3)	; initial print level used
(defvar *rep-plen* 10)	; initial print length used
					   
					     
; repair a symbol -- the generic entry point

(defmacro repair (a)
    (unless (symbolp a) (error "~s is not a symbol" a))
    (let
     ((*breakenable* nil)
      (*rep-exit* 0)
      (*rep-name* (cons "symbol" a))
      (*print-level* *rep-plev*)
      (*print-length* *rep-plen*))
     (catch 'abort (rep-rep a)))
     `',a)

; repair a function, with editable arguments

(defmacro repairf (a)
    (let
     ((*breakenable* nil)
      (*rep-exit* 0) 
      (*rep-name* (cons "function" a)) 
      (*print-level* *rep-plev*)
      (*print-length* *rep-plen*))
     (catch 'abort
	    (if (fboundp a)
		(let ((x (rep-rep(get-lambda-expression(symbol-function a)))))
		     (case (first x)
			   (lambda `(defun ,a ,@(rest x)))
			   (macro  `(defmacro ,a ,@(rest x)))
			   (t (error "not a closure!"))))
		(error "can't repair")))))


; rep-propp returns T if p is a property of a

(defun rep-propp (a p)
	(do 	((plist (symbol-plist a) (cddr plist)))
		((or (null plist) (eq (car plist) p))
		 (not (null plist)))))

; terminate input line

(defun rep-teread (error) 
	(if (not (eq (peek-char) #\Newline))
	    (read-line))
	(if error
	    (princ "Try again:")
	    (format t "~a ~a>" (car *rep-name*) (cdr *rep-name*))))

(defmacro rep-protread () ;;Protected read -- we handle errors
	'(do ((val (errset (read)) 
		  (progn (rep-teread t) (errset (read)))))
	    ((consp val) (car val))))

(defmacro rep-proteval () ;;protected eval -- we handle errors
			  ;; we also use evalhook so environment is global
			  ;;  plus a local @, which cannot be changed!
	'(do* ((env (cons (list (list (cons '@ list))) nil))
	       (val (errset (evalhook (read) nil nil env))
		    (progn (rep-teread t) 
			   (errset (evalhook (read) nil nil env)))))
	      ((consp val) (car val))))


; New methods so that we can "repair" methods.
; selectors :get-messages, :get-ivars, and :get-super changed to 
; :messages, :ivars, and :superclass to be compatible with new classes.lsp.

(send Class :answer :messages '() '(messages))

(send Class :answer :set-messages '(value) '((setf messages value)))

; new methods so that we can examine/change instance variables

(send Class :answer :ivars '() '(ivars))

(send Class :answer :superclass '() '(superclass))

(defun rep-ivar-list (obj &aux (cls (send obj :class)))
    (do ((ivars (send cls :ivars)
	        (append (send super :ivars) ivars))
	 (super (send cls :superclass) (send super :superclass)))
	((null super) ivars)
     ))

#+:packages (import '(xlisp::%struct-ref xlisp::%struct-set))

(defun rep-struct (struct &aux (count 0))
       (map 'list
	    #'(lambda (x)
		      (list (first x)
			    (%struct-ref struct (setq count (1+ count)))
			    ))
	    (get (type-of struct) '*struct-slots*)))

(defun rep-set-struct (nlist struct
			     &aux (slots (get (type-of struct)
					      '*struct-slots*)))
       (mapc #'(lambda (x)
		       (when (and (consp x)
				  (member (car x) slots :key #'car))
			     (%struct-set struct
					  (1+ (position (car x)
							slots
							:key #'car))
					  (cadr x))))
	     nlist)
       struct)
       
#+:packages (unintern 'xlisp::%struct-ref)
#+:packages (unintern 'xlisp::%struct-set)


(defun rep-ivars (list obj)
    (mapcar #'(lambda (x)
	        (let ((y (errset (apply #'send
				        (list obj
#-:packages		     		      (intern (strcat ":"
							      (string x)))
#+:packages     			      (intern (string x) :keyword)
					      ))
				 nil)))
		  (if (consp y) (list x (car y)) x)))
	    list))

(defun rep-set-ivars (alist obj)
    (mapc #'(lambda (x)
	      (if (consp x)
		  (let ((y (errset (apply #'send
					  (list obj
					        :set-ivar
						(car x)
					        (cadr x)))
				   nil)))
		    (unless (consp y)
			    (princ (list (car x) " not set."))
			    (terpri)))
		  (progn (princ (list x "not set.")) (terpri))))
	  alist))

; help function
(defun rep-help (list)
       (terpri)
       (princ "Available commands:\n\n")
       (princ ":?\t\tprint list of commands\n")
       (princ ":RETURN\t\texit, saving all changes\n")
       (princ ":ABORT\t\texit, without changes\n")
       (princ ":BACK\t\tgo back one level (as before CAR CDR or N commands)\n")
       (princ ":B n\t\tgo back n levels\n")
       (cond ((symbolp list)
	      (princ ":L\t\tshort description of selected symbol\n")
	      (princ ":MAP\t\tcomplete description of selected symbols properties\n"))
	     ((consp list)
	      (princ ":L\t\tshow selection (using pprint)\n")
	      (princ ":MAP\t\tpprints each element of selection\n"))
	     (t 
	      (princ ":L\t\tshow selection (using pprint)\n")
	      (princ ":MAP\t\tshow selection (using pprint)\n")))
       (format
	t 
	":PLEV n\t\tsets number of levels of printing (now ~s) NIL=infinite\n"
	*print-level*)
       (format
	t
	":PLEN n\t\tsets length of list printing (now ~s) NIL=infinite\n"
	*print-length*)
       (princ ":EVAL x\t\tevaluates x and prints result\n")
       (princ "\t\tNote the symbol tools:@ is bound to the selection\n")
       (princ ":REPLACE x\treplaces the selection with evaluated x\n")
       (princ "\t\tNote the symbol tools:@ is bound to the selection\n")
       (when (symbolp list)
	     (princ ":FUNCTION\tedit the function binding\n")
	     (princ ":VALUE\t\tedit the value binding\n")
	     (princ ":PROP pname\tedit property pname\n")
	     (return-from rep-help nil))
       (unless (consp list) (return-from rep-help nil))
       (princ ":CAR\t\tSelect the CAR of the selection\n")
       (princ ":CDR\t\tSelect the CDR of the selection\n")
       (princ "n\t\tSelect the nth element in the selection (0 based)\n")
       (princ ":SUBST x y\tall EQUAL occurances of y are replaced with x\n")
       (princ ":RAISE n\tremoves parenthesis surrounding nth element of the selection\n")
       (princ ":LOWER n m\tinserts parenthesis starting with the nth element,\n")
       (princ "\t\tfor m elements of the selection\n")
       (princ ":ARRAY n m\tas in LOWER, but makes elements into an array\n")
       (princ ":I n x\t\tinserts (quoted) x before nth element in selection\n")
       (princ ":R n x\t\treplaces nth element in selection with (quoted) x\n")
       (princ ":D n\t\tdeletes nth element in selection\n"))


; rep-rep repairs its argument.  It looks at the argument type to decide
;  how to do the repair.
;  ARRAY  -- repair as list
;  OBJECT -- if class, repair MESSAGE ivar, else repair list of ivars
;  CLOSURE -- allows repairing of closure body by destructive modification
;             upon return
;  OTHER  -- repair as is.

(defun rep-rep (list) 
	(cond ((arrayp list) 
	       (format t "Editing array~%") 
	       (coerce (rep-rep2 (coerce list 'cons)) 'array))
	      ((classp list)
	       (format t "Editing Methods~%")
	       (send list :set-messages 
		      	  (rep-rep2 (send list :messages)))
	       list) ; return the object
	      ((objectp list)
	       (format t "Editing Instance Vars~%")
	       (rep-set-ivars (rep-rep2 
	                       (rep-ivars 
			        (rep-ivar-list list) list)) list)
	       list) ; return the object
	      ((typep list 'struct)
	       (format t "Editing structure~%")
	       (rep-set-struct (rep-rep2 (rep-struct list)) list))
	      ((typep list 'closure)
	       (format t "Editing closure~%")
	       (let*  ((orig (get-lambda-expression list))
	               (new (rep-rep2 orig)))
		      (when (not (equal (second orig) (second new)))
		      	    (princ "Argument list unchanged")
			    (terpri))
		      (rplaca (cddr orig) (caddr new))
		      (rplacd (cddr orig) (cdddr new))
		      list)) ; return closure
	      (t (rep-rep2 list))))


; printing routines

; print a property list
(defun rep-print-prop (plist verbosity)
	(when plist
	      (format t "Property: ~s" (first plist))
	      (when verbosity
		    (format t "   ~s" (second plist)))
	      (terpri)
	      (rep-print-prop (cddr plist) verbosity)))

; print a symbols function binding, value, and property list
(defun rep-print-symbol (symbol verbosity)
       (format t "Print name: ~s~%" symbol)
       (unless (null symbol)
	(when (fboundp symbol)
	      (if verbosity 
		  (if (typep (symbol-function symbol) 'closure)
		      (progn
		       (format t "Function:~%")
		       (pprint (get-lambda-expression
				(symbol-function symbol))))
		      (format t "Function: ~s~%" (symbol-function symbol)))
		  (format t "Function binding~%")))
	(when (boundp symbol)
	      (if (constantp symbol) 
		  (princ "Constant V")
		  (princ "V"))
	      (if verbosity
		  (if (< (flatsize (symbol-value symbol)) 60)
		      (format t "alue: ~s~%" (symbol-value symbol))
		      (progn
		       (format t "alue:~%")
		       (pprint (symbol-value symbol))))
		  (format t "alue binding~%")))
	(when (symbol-plist symbol)
	      (rep-print-prop (symbol-plist symbol) verbosity)))
)

; print a list, using mapcar
(defun rep-print-map (list &aux (x 0))
       (mapc #'(lambda (y)
		       (format t "(~s) " (prog1 x (setf x (1+ x)) ))
		       (pprint y))
	     list))

; main list repair interface
(defun rep-rep2 (list) 
    (prog (command n)
	y (rep-teread nil)
	  (setq command (rep-protread))
	  ;; When packages installed, we will convert symbol names
	  ;; entered as commands into keywords
	  ;; This *does* clutter the current package symbol list
	  #+:packages(when (and (symbolp command)
				(not (eq (symbol-package command)
					 (find-package :keyword))))
			   (setq command
				 (intern (string command)
					 :keyword)))
	  (cond	((eq command :?) (rep-help list))
		((eq command :return) (setq *rep-exit* -1))
		((eq command :abort) (throw 'abort))
		((eq command :back) (return list))
		((and (eq command :b)
		      (integerp (setq n (rep-protread)))
		      (> n 0))
		 (setq *rep-exit* n))
		((eq command :l)
		 (if (symbolp list) (rep-print-symbol list nil) (print list)))
		((eq command :map)
		 (cond ((symbolp list) (rep-print-symbol list t))
		       ((consp list) (rep-print-map list))
		       (t (pprint list))))
		((eq command :eval) (print (rep-proteval)))
		((and (eq command :plev)
		      (or (and (integerp (setq n (rep-protread)))
			       (>= n 1))
			  (null n)))
		 (format t "Was ~s\n" *print-level*)
		 (setq *print-level* n))
		((and (eq command :plen)
		      (or (and (integerp (setq n (rep-protread)))
			       (>= n 1))
			  (null n)))
		 (format t "Was ~s\n" *print-length*)
		 (setq *print-length* n))
		((eq command :replace) 
		 (setq n (rep-proteval))
		 (if (eq (type-of n) (type-of list))
		     (setq list n)
		     (return (rep-rep n))))
; symbol only commands
		((and (symbolp list)
		      (eq command :function) 
		      (fboundp list)
		      (typep (symbol-function list) 'closure))
		 (let ((*rep-name* (cons "function" list)))
			(setf (symbol-function list) 
			      (rep-rep (symbol-function list)))))
		((and (symbolp list)
		      (eq command :value)
		      (boundp list)
		      (null (constantp list)))
		 (let ((*rep-name* (cons "value" list)))
			(setf (symbol-value list)
			      (rep-rep (symbol-value list)))))
		((and (symbolp list)
		      (eq command :prop)
		      (symbolp (setq n (rep-protread)))
		      (rep-propp list n))
		 (let ((*rep-name* (cons n list)))
			(setf (get list n) (rep-rep (get list n)))))
; cons only commands
		((and (consp list)
		      (eq command :car))
		 (setq list (cons (rep-rep (car list)) (cdr list))))
		((and (consp list)
		      (eq command :cdr))
		 (setq list (cons (car list) (rep-rep (cdr list)))))
		((and (consp list)
		      (integerp command)
		      (> command -1) 
		      (< command (length list)))
		 (setq list (append
			     (subseq list 0 command)
			     (list (rep-rep (nth command list)))
			     (nthcdr (1+ command) list))))
		((and (consp list)
		      (eq command :raise) 
		      (integerp (setq n (rep-protread)))
		      (> n -1) 
		      (< n (length list))
		      (or (consp (nth n list)) (arrayp (nth n list))))
		 (setq list (append
			     (subseq list 0 n)
			     (let ((x (nth  n list)))
				  (if (arrayp x)
				      (coerce x 'cons)
				      x))
			     (nthcdr (1+ n) list))))
		((and (consp list)
		      (eq command :lower)
		      (integerp (setq n (rep-protread)))
		      (> n -1)
		      (integerp (setq n2 (rep-protread)))
		      (> n2 0)
		      (>= (length list) (+ n n2)))
		 (setq list (append
			     (subseq list 0 n)
			     (list (subseq list n (+ n n2)))
			     (nthcdr (+ n n2) list))))
		((and (consp list)
		      (eq command :array)
		      (integerp (setq n (rep-protread)))
		      (> n -1)
		      (integerp (setq n2 (rep-protread)))
		      (> n2 0)
		      (>= (length list) (+ n n2)))
		 (setq list (append
			     (subseq list 0 n)
			     (list (coerce (subseq list n (+ n n2)) 'array))
			     (nthcdr (+ n n2) list))))
		((and (consp list)
		      (eq command :i) 
		      (integerp (setq n (rep-protread)))
		      (> n -1))
		 (setq list (append
			     (subseq list 0 n)
			     (list (rep-protread))
			     (nthcdr n list))))
		((and (consp list)
		      (eq command :r) 
		      (integerp (setq n (rep-protread)))
		      (> n -1))
		 (setq list (append
			     (subseq list 0 n)
			     (list (rep-protread))
			     (nthcdr (1+ n) list))))
		((and (consp list)
		      (eq command :d) 
		      (integerp (setq n (rep-protread)))
		      (> n -1))
		 (setq list (append
			     (subseq list 0 n)
			     (nthcdr (1+ n) list))))
		((and (consp list)
		      (eq command :subst))
		 (setq list (subst (rep-protread) 
				   (rep-protread) 
				   list
				   :test #'equal)))
		(t (princ "What??\n") (go y)))

	  (when (zerop *rep-exit*) (go y))
	  (setq *rep-exit* (1- *rep-exit*))
	  (return list)))

