; useful stuff for object programming

(in-package "XLISP")

(export '(defclass defmethod definst classp tracemethod untracemethod))

; filter certain keyword arguments for passing argument list to superclass

(defun remove-keys (keys list)
    (cond ((null keys) list)
	  ((null list) 'nil)
	  ((member (car list) keys)
	   (remove-keys (remove (car list) keys) (cddr list)))
	  (t (cons (car list) (remove-keys keys (cdr list))))))


; fix so that classes can be named (requires PNAME ivar in class Class)
;  The source files have been modified for PNAME instance variable,
;  and printing of class PNAME if it exists.

(send class :answer :set-pname
      '(name)
      '((setf pname (string name))))


; *setf* property of SEND is set to allow setting instance variables

(setf (get 'send '*setf*) 
      #'(lambda (obj ivar value) 
		(send obj :set-ivar
#-:packages	      (get ivar 'ivarname)
#+:packages	      ivar
		      value)))

; (defclass <classname> [(<instvars>) [(<classvars>) [<superclass>]]])
; defclass sets up access methods for all instance and class variables!
; an instance variable can be of form <ivar>  or (<ivar> <init>)
; :ISNEW is automatically defined to accept keyword arguments to overide
; default initialization.

(defmacro defclass (name &optional ivars cvars super 
			 &aux (sym (gensym)) (sym2 (gensym)))
; CIVAR is instance variable list with init values removed
    (let ((civars (mapcar #'(lambda (x) (if (consp x) (car x) x))
			  ivars)))

      `(progn ; Create class and assign to global variable
              (setf ,name
		    (send class :new
			  ',civars
			  ',cvars
			  ,@(if super (list super) nil)))

	      ; Set the name ivar of the class
	      (send ,name :set-pname ',name)

	      ; Generate the :<ivar> and :<cvar> methods
	      ,@(mapcar #'(lambda (arg)
			    `(send ,name
				   :answer
#-:packages     		   ,(intern (strcat ":" (string arg)))
#+:packages     		   ,(intern (string arg) :keyword)
				   'nil
				   '(,arg)))
		        (append civars cvars))

	      ; The method needed to set the instance variables
	      (send ,name :answer :set-ivar
		    '(,sym ,sym2)
		    '((case ,sym
			    ,@(mapcar #'(lambda (arg)
#-:packages				        `(,arg (setq ,arg ,sym2))
#+:packages					`(,(intern (string arg)
							   :keyword)
						  (setq ,arg ,sym2))
						)
				      (append civars cvars))
			    (t (send-super :set-ivar ,sym ,sym2)))))

	      ; Set the ivarname property of the :<ivar> symbols
		  ,@(mapcar #'(lambda (arg)
							  `(setf (get
#-:packages		    		    ',(intern (strcat ":" (string arg)))
#+:packages					    ',(intern (string arg) :keyword)
									  'ivarname)
									 ',arg))
					civars)

	      ; Generate the :ISNEW method
	      (send ,name
		    :answer :isnew
		    '(&rest ,sym &key ,@ivars &allow-other-keys)

		    ; first :ISNEW setfs 
		    ;  for all its declared instance variables
		    '(,@(mapcar #'(lambda (arg)
				    `(setf (send self
#-:packages     			   	 ,(intern (strcat ":" 
							   (string arg)))
#+:packages     				 ,(intern (string arg)
							  :keyword)
						 )
					   ,arg))
			        civars)

		      ; then the remaining initialization arguments are
		      ;  passed to the superclass.
		      (apply #'send-super
			     (cons ':isnew
				   (remove-keys
				      ',(mapcar #'(lambda (arg)
#-:packages     				    (intern (strcat ":"
							       (string arg)))
#+:packages     				    (intern (string arg)
							    :keyword)
							  )
					        civars)
				      ,sym)))
		      self))
	      ,name)))


; (defmethod <class> <message> (<arglist>) <body>)

(defmacro defmethod (cls message arglist &rest body)
    `(send ,cls
	   :answer
	   ,message
	   ',arglist
	   ',body))

; (definst <class> <instname> [<args>...])

(defmacro definst (cls name &rest args)
    `(setf ,name
           (send ,cls
	         :new
		 ,@args)))

; (extensions suggested by Jim Ferrans)

(defmethod class :superclass () superclass)
(defmethod class :messages () messages)

(defmethod object :superclass () nil)

(defmethod object :ismemberof (cls)
	   (eq (send self :class) cls))

(defmethod object :iskindof (cls)
	   (do ((this (send self :class) (send this :superclass)))
	       ((or (null this)(eq this cls))
		(eq this cls))))

(defmethod object :respondsto (selector &aux temp)
	   (do ((this (send self :class) (send this :superclass)))
	       ((or (null this)
		    (setq temp 
			  (not (null (assoc selector 
				       (send this :messages))))))
		temp)
	       (setf temp nil)))


(defmethod class :ivars () ivars)

(defmethod class :pname () pname)

; :Storeon returns a list that can be executed to re-generate the object.
; It relies on the object's class being created using DEFCLASS,   so the
; instance variables can be generated.


(defmethod object :storeon (&aux cls ivlist res)
	   (setq cls
		 (send self :class)
		 ivlist
		 (do ((ivars (send cls :ivars)
			     (append (send super :ivars) ivars))
		      (super (send cls :superclass)
			     (send super :superclass)))
		     ((eq super object) ivars))
		 res
		 (mapcan #'(lambda (x) 
				   (let ((temp
#-:packages     			  (intern (strcat ":" (string x)))
#+:packages     			  (intern (string x) :keyword)
					  ))
					(list temp
					      (let ((y (send self temp)))
						   (if (and y 
							    (or (symbolp y)
								(consp y)))
						       (list 'quote y)
						       y)))))
				   ivlist))
	   (append (list 'send (intern (send cls :pname)) ':new)
		   res))

; For classes we must use a different tact.
; We will return a PROGN that uses SENDs to create the class and any methods.
; It also assumes the global environment. None of the DEFxxx functions
; are needed to do this.

; because of the subrs used in messages, :storeon cannot be  used to
; generate a reconstructable copy of classes Object and Class.

; Class variables are not set, because there are no class methods in XLISP
; to do this (one would have to create an instance, and send messages to
; the instance, and I feel that is going too far).


(defmethod class :storeon (&aux (classname (intern pname)))
   (nconc (list 'progn)
	  (list (list 'setq classname
		      (list 'send 'class :new ivars cvars 
			    (if superclass 
				(intern (send superclass :pname))
				nil))))
	  (list (list 'send classname :set-pname pname))
	  (mapcar #'(lambda (mess &aux 
				  (val (if (typep (cdr mess) 'closure)
					   (get-lambda-expression (cdr mess))
					   (list nil nil mess))))
			    (list 'send classname :answer
				  (first mess)
				  (list 'quote (cdadr val))
				  (list 'quote (cddr val))))
		  messages)))


; The following functions, TRACEMETHOD and UNTRACEMETHOD, require
; XLISP-PLUS 3.0 or later to use.

(defun tracemethod (cls &optional selector)
; Add selector selector in class cls to the tracelist.
; Optionally, only the class can be specified, and all methods will be traced
; Returns tracelist if successful.
       (if selector
	   (when (and (objectp cls)
		      (eq (send cls :class) Class)
		      (member-if (lambda (x) (eq (car x) selector))
				 (send cls :messages)))
		 (if (member (list cls selector) *tracelist* :test #'equal)
		     *tracelist* ; Success without adding entry
		     (setq *tracelist* (cons (list cls selector) *tracelist*))))
	   (when (and (objectp cls) (eq (send cls :class) Class))
		 (mapc (lambda (x) (tracemethod cls (car x)))
		       (send cls :messages))
		 *tracelist*)))

(defun untracemethod (&optional cls selector)
; (untracemethod) will remove all methods from the tracelist
; (untracemethod class) will remove all methods in class from the tracelist
; (untracemethod class selector) will remove single method from trace list
       (setq *tracelist*
	     (if selector
		 (delete (list cls selector) *tracelist* :test #'equal)
		 (if cls
		     (delete-if (lambda (x) (and (consp x)(eq (first x) cls)))
				*tracelist*)
		     (delete-if #'consp *tracelist*)))))

(setq *features* (cons :classes *features*))
