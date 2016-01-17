;;;;
;;;; Additional Common Lisp Functions for XLISP-STAT 2.0
;;;; XLISP-STAT 2.1 Copyright (c) 1990-95, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;

(in-package "XLISP")

;;;;;;
;;;;;;         Environment Access and Modification Routines
;;;;;;

(defvar *cmp-global-macros* nil)
(defvar *cmp-macros*)
(defvar *cmp-specials*)

(export '(ftype ignorable ignore inline notinline))


;;;;
;;;; PARSE-MACRO and ENCLOSE
;;;;

(export '(enclose parse-macro))

(defun split-declarations (x)
  (flet ((head-is-declaration (x)
	   (and (consp (first x)) (eq (first (first x)) 'declare)))
	 (head-is-docstring (x) (and (stringp (first x)) (consp (rest x))))
	 (check-declarations (decls)
	   (dolist (d decls)
	     (dolist (i (rest d))
	       (if (and (consp i) (eq (first i) 'special))
		   (dolist (v (rest i))
		     (warn "special declaration for ~s ignored." v)))))))
    (do ((decls nil)
	 (body x (rest body))
	 (doc nil))
	(nil)
	(cond
	 ((head-is-declaration body) (push (first body) decls))
	 ((head-is-docstring body) (setf doc (first body)))
	 (t (check-declarations decls)
	    (return (list (nreverse decls) body doc)))))))

;; This returns a lambda expression consistent with the CLtL2
;; specification of macro expansion functions as functions of two
;; arguments, the form to be expanded and the environment to use. The
;; internal calling conventions and construction of macro closures has
;; been modified accordingly.  This definition does not do the right
;; thing with declarations at this point.
(defun make-macro-lambda-expression (name lambda-list body)
  (flet ((fixup-macro-args (args)
           (let ((esym nil)
		 (wsym nil))
	     (setf args (copy-list args))
	     (let ((last (last args)))
	       (unless (null (cdr last))
		       (rplacd last (list '&rest (cdr last)))))
	     (setf args (nsubst '&rest '&body args))
	     (when (find '&environment args)
		   (setf esym (second (member '&environment args)))
		   (setf args (remove esym (remove '&environment args))))
	     (cond
	      ((eq (first args) '&whole)
	       (setf wsym (second args))
	       (setf args (rest (rest args))))
	      (t (setf wsym (gensym "WHOLE"))))
	     (cons esym (cons wsym args))))
	 (destructuring-arglist-p (x)
           (consp
	    (find-if #'(lambda (x)
			 (or (member x lambda-list-keywords) (consp x)))
		     x))))
    (let* ((args (fixup-macro-args lambda-list))
	   (bd (split-declarations body))
	   (body `(,@(first bd) (block ,name ,@(second bd))))
	   (doc (third bd))
	   (wsym (second args))
	   (esym (first args))
	   (edecl nil)
	   (llist (rest (rest args))))
      (unless esym
	      (setf esym (gensym "ENV"))
	      (setf edecl `((declare (ignore ,esym)))))
      (values
       `(lambda (,wsym ,esym)
	  ,@edecl
	  ,(if (destructuring-arglist-p args)
	       `(destructuring-bind ,llist (rest ,wsym) ,@body)
	       `(apply #'(lambda ,llist ,@body) (rest ,wsym))))
       doc))))


;; This implementation produces an interpreted function
(defun enclose (expr &optional env) (evalhook expr nil nil env))

(defun parse-macro (name lambda-list body  &optional env)
  (enclose (make-macro-lambda-expression name lambda-list body) env))


;;;;
;;;; DEFINE-COMPILER-MACRO
;;;;
;;;; Fake version to be overridded if the compiler is loaded

(export 'define-compiler-macro)

(defmacro set-cmp-macro (symarg funarg)
  `(let* ((sym ,symarg)
	  (fun ,funarg)
	  (entry (assoc sym *cmp-global-macros*)))
     (if entry
	 (rplacd entry fun)
         (push (cons sym fun) *cmp-global-macros*))
     sym))

(defmacro define-compiler-macro (sym args &rest body)
  (let ((fexpr (make-macro-lambda-expression sym args body)))
    `(progn
       (set-cmp-macro ',sym (coerce-to-macro #',fexpr))
       ',sym)))

(defmacro define-special-form-macro (sym args &rest body)
  (let ((fexpr (make-macro-lambda-expression sym args body)))
    `(progn
       (%set-get ',sym 'macro (coerce-to-macro #',fexpr))
       ',sym)))


;;;;
;;;; AUGMENT-ENVIRONMENT, VARIABLE-INFORMATION, and FUNCTION-INFORMATION
;;;;

(export '(augment-environment variable-information function-information))

;; Environments are of the form (venv . fenv). Each of venv and fenv
;; is a list of frames, and frames are lists of bindings. Bindings are
;; pairs (name . value).  If the value in a variable binding is
;; :symbol-macro and the next pair has name :symbol-macro, then the
;; value of the second of these bindings represente the symbol macro
;; expansion of the symbol in the first binding.

(defun special-variable-p (sym)
  (or (specialp sym)
      (and (boundp '*cmp-specials*) (member sym *cmp-specials*))))

(defun augment-environment (env &key
				variable symbol-macro function macro declare)
  (declare (ignore declare))
  (when (intersection variable (mapcar #'first symbol-macro))
	(error "adding some symbols as variables and as symbol macros"))
  (when (intersection function (mapcar #'first macro))
	(error "adding some symbols as functions and macros"))
  (dolist (v variable)
    (when (special-variable-p v)
          (error "cant't make lexical binding for the special variable ~s" v)))
  (dolist (sm symbol-macro)
    (let ((v (first sm)))
      (when (special-variable-p v)
	    (error "cant't make lexical binding for the special variable ~s"
		   v))))
  (flet ((add-variable-frame (frame env)
	   (if frame (cons (cons frame (car env)) (cdr env)) env))
	 (add-function-frame (frame env)
	   (if frame (cons (car env) (cons frame (cdr env))) env))
	 (make-variable-frame (vars) (mapcar #'list vars))
	 (make-function-frame (funs) (mapcar #'list funs))
	 (make-macro-frame (macs)
	   (mapcar #'(lambda (x) (cons (first x) (coerce-to-macro (second x))))
		   macs))
	 (make-symbol-macro-frame (smacs)
	   (let ((frame nil))
	     (dolist (s smacs)
	       (push (cons :symbol-macro (second s)) frame)
	       (push (cons (first s) :symbol-macro) frame))
	     frame)))
    (setf env (add-variable-frame (make-variable-frame variable) env))
    (setf env (add-variable-frame (make-symbol-macro-frame symbol-macro) env))
    (setf env (add-function-frame (make-function-frame function) env))
    (setf env (add-function-frame (make-macro-frame macro) env))
    env))

(defun get-environment-entry (name env)
  (dolist (frame env)
    (let ((m (member name frame :key #'car)))
      (when m
	    (if (and (eq (cdr (first m)) :symbol-macro)
		     (eq (car (second m)) :symbol-macro))
		(return (values t :symbol-macro (second m) frame))
	      (return (values t (cdr (first m)) nil frame)))))))

(defun global-symbol-macro (sym)
  (if (or (boundp sym) (special-variable-p sym))
      (values nil nil)
    (let ((m (member :symbol-macro (symbol-plist sym))))
      (if m
	  (values (second m) t)
	(values nil nil)))))

;;**** this needs fixing once special declarations are supported
(defun variable-information (sym &optional env)
  (multiple-value-bind (found value extra)
		       (get-environment-entry sym (first env))
    (cond
     (found
      (if (and found (eq value :symbol-macro) (eq (car extra) :symbol-macro))
	  (values :symbol-macro t nil)
	(values :lexical t nil)))
     ((constantp sym) (values :constant nil nil))
     ((special-variable-p sym) (values :special nil nil))
     ((nth-value 1 (global-symbol-macro sym))
      (values :symbol-macro nil nil))
     (t (values nil nil nil)))))

;;**** needs to be fixed for special forms
;;**** needs to be fixed for C-compiler macroe is they ever materialize
(defun function-information (sym &optional env)
  (flet ((macrop (f)
	   (and (or (typep f 'byte-code-closure) (typep f 'closure))
		(not (functionp f))))
	 (global-frame-p (f)
	   (or (eq f *cmp-global-macros*)
	       (and (boundp '*cmp-macros*)
		    (eq f *cmp-macros*)))))
    (multiple-value-bind (found value extra frame)
			 (get-environment-entry sym (rest env))
     (declare (ignore extra))
     (cond
      ((and found (macrop value))
       (values :macro (not (global-frame-p frame)) nil))
      (found (values :function t nil))
      ((and (fboundp sym) (macrop (symbol-function sym)))
       (values :macro nil nil))
      ((and (fboundp sym) (typep (symbol-function sym) 'fsubr))
       (values :special-form nil nil))
      ((and (fboundp sym) (functionp (symbol-function sym)))
       (values :function nil nil))
      (t (values nil nil nil))))))


;;;;
;;;; New versions of MACROEXPAND and MACROEXPAND-1 that handle symbol macros
;;;;

(export '(macroexpand macroexpand-1))

;; This function copies symbol macro forms to protect them from
;; splicing that occurs if *displace-macros* in non-nil.
(defun macroexpand-symbol-1 (symbol &optional env)
  (unless (symbolp symbol) (error "not a symbol - ~s" symbol))
  (multiple-value-bind (found value extra)
		       (get-environment-entry symbol (first env))
    (if (and found (eq value :symbol-macro) (eq (car extra) :symbol-macro))
      (let ((form (cdr extra)))
	(values (if (consp form) (copy-tree form) form) t))
     (multiple-value-bind (form found)
			  (global-symbol-macro symbol)
       (if found
	   (values (if (consp form) (copy-tree form) form) t)
	 (values symbol nil))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
	   (unless (fboundp 'base-macroexpand-1)
		   (setf (symbol-function 'base-macroexpand-1)
			 #'macroexpand-1)))

;; new version that expands symbol macros
(defun macroexpand-1 (form &optional env)
  (if (symbolp form)
      (macroexpand-symbol-1 form env)
      (base-macroexpand-1 form env)))

;; new version that expands symbol macros
(defun macroexpand (form &optional env)
  (multiple-value-bind
   (form expanded)
   (macroexpand-1 form env)
   (if expanded
       (loop
	(multiple-value-bind
	 (newform expanded)
	 (macroexpand-1 form env)
	 (unless expanded (return (values newform t)))
	 (setf form newform)))
       (values form nil))))


;;;;
;;;; DEFINE-SYMBOL-MACRO
;;;;

(export 'define-symbol-macro)

(defmacro define-symbol-macro (name form)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
	      (when (or (special-variable-p ',name) (boundp ',name))
		    (error "can't assign symbol macro to ~s" ',name))
	      (setf (get ',name :symbol-macro) ',form)
	      ',name))


;;;;;
;;;;;                   New Setf Macro Expansion System
;;;;;

(export '(get-setf-method setf))

(defun get-cmp-setf-info (sym &optional env)
  (declare (ignore env))
  (if (boundp '*cmp-setf*) (assoc sym *cmp-setf*)))

(defun get-cmp-setf-method (sym &optional env)
  (let ((cmpsetf (get-cmp-setf-info sym env)))
    (if (and cmpsetf (eq (second cmpsetf) '*setf*)) (third cmpsetf))))

(defun get-cmp-setf-lambda (sym &optional env)
  (let ((cmpsetf (get-cmp-setf-info sym env)))
    (if (and cmpsetf (eq (second cmpsetf) '*setf-lambda*)) (third cmpsetf))))

(defun get-xlisp-setf-method (sym &optional env)
  (declare (ignore env))
  (get sym '*setf*))

(defun get-xlisp-setf-lambda (sym &optional env)
  (declare (ignore env))
  (get sym '*setf-lambda*))

;; This function checks for defsetf expanders before doing macro expansion,
;; so one can defsetf a macro. This seems to be what many implementations
;; do, but I'm not sure it is consistent with CLtL2.
(defun get-setf-method (form &optional env)
  (loop
   (cond
    ((consp form)
     (let* ((sym (first form))
	    (is-global (not (nth-value 1 (function-information sym env))))
	    (cmpsfun (get-cmp-setf-method sym env))
	    (cmpslam (get-cmp-setf-lambda sym env))
	    (sxfun (get-xlisp-setf-method sym env))
	    (sxlam (get-xlisp-setf-lambda sym env)))
       (when (and (or cmpsfun cmpslam sxfun sxlam) is-global)
	     (let* ((args (rest form))
		    (tvars (mapcar #'(lambda (x) (gensym "T")) args))
		    (vvars (list (gensym "V")))
		    (aform `(,sym ,@tvars)))
	       (return
		(values tvars
			args
			vvars
			(cond
			 (cmpsfun `(,cmpsfun ,@tvars ,@vvars))
			 (cmpslam (apply cmpslam (append tvars vvars)))
			 ((and sxfun (symbolp sxfun))
			  `(,sxfun ,@tvars ,@vvars))
			 (sxfun
			  `(funcall (get ',sym '*setf*) ,@tvars ,@vvars))
			 (sxlam (apply sxlam (append tvars vvars))))
			aform))))
       (when (and (eq sym 'apply) is-global)
	     (return (get-apply-setf-method form env)))
       (multiple-value-bind (newform changed) (macroexpand-1 form env)
	 (unless changed (error "bad place form - ~s" form))
	 (setf form newform))))
    ((symbolp form)
     (multiple-value-bind (newform changed) (macroexpand-1 form env)
       (cond
	(changed (setf form newform))
	(t
	 (let ((sym (gensym (string form))))
	   (return (values nil nil `(,sym) `(setq ,newform ,sym) newform)))))))
    (t (error "bad place form - ~s" form)))))

;; The implementation here seems consistent with AKCL and common
;; sense, but not with CLtL. The specification in CLtL would not work
;; for things with setf methods defined by the simple form of defsetf
(defun get-apply-setf-method (form &optional env)
  (declare (ignore env))
  (let ((fun (second form)))
    (unless (and (consp fun)
		 (eq (first fun) 'function)
		 (symbolp (second fun))
		 (null (rest (rest fun))))
	    (error "bad place form - ~s" form)))
  (multiple-value-bind
   (tvars tvals vvars sform aform)
   (get-setf-method `(,(second (second form)) ,@(rest (rest form))))
   (cond
    ((equal (last tvars) (last sform)) ;; simple case as described in CLtL
     (values tvars
	     tvals
	     vvars
	     `(apply #',(first sform) ,@(rest sform))
	     `(apply #',(first aform) ,@(rest aform))))
    ((equal (last tvars) (last (butlast sform))) ;; covers simple defsetf's
     (values tvars
	     tvals
	     vvars
	     (let* ((lv (first (last sform)))
		    (bl (butlast sform))
		    (rv (first (last bl))))
	       `(apply #',(first sform)
		       ,@(butlast (rest bl))
		       (append ,rv (list ,lv))))
	     `(apply #',(first aform) ,@(rest aform))))
    (t (error "bad place form - ~s" form)))))    


;; The special variable *simplify-setf* controls the tradeoff of speed
;; and correctness used by make-setf-form in expanding setf forms. If
;; this variable is nil, then make-setf-form is careful about avoiding
;; multiple evaluation and doing evaluations in the proper order.  If
;; *simplify-setf* is not nil, then subforms of place forms and the
;; value form are not protect against multiple evaluation occurring in
;; setf expansions. This is consistent with the internal
;; implementation. It should not be a problem for any f the standard
;; setf methods. The compiler should be able to optimize out all
;; unnecessary variables the careful version setsup, so the compiler
;; should probably bind this variable to nil.

(export '*simplify-setf*)

(defvar *simplify-setf* nil)

(defun make-setf-form (form value &optional env)
  (multiple-value-bind (tvars args vvars sform)
		       (get-setf-method form env)
    (let ((vars (append tvars vvars))
	  (vals (append args (list value))))
      (if *simplify-setf*
	  (sublis (mapcar #'cons vars vals) sform)
	`(let* ,(mapcar #'list vars vals) ,sform)))))

(defmacro setf (&rest pairs &environment env)
  (let ((forms nil))
    (loop
     (when (null pairs)
	   (return (if (consp (rest forms))
		       `(progn ,@(nreverse forms))
		       (first forms))))
     (unless (consp (rest pairs))
	     (error "setf requires an even number of arguments"))
     ;; need to use setq here to avoid infinite recursion
     (setq forms
	   (cons (make-setf-form (first pairs) (second pairs) env) forms))
     (setq pairs (rest (rest pairs))))))

;;;;;;
;;;;;; New versions of Some Modifier Macros
;;;;;;
;;;;;; These versions (except rotatef) attempt to optimize simple
;;;;;; symbol place forms with so symbol macro bindings.

(export '(incf decf push pop pushnew remf rotatef))

;; this checks if place is a symbol with no macro definition in env
(defun non-macro-variable-symbol-p (place env)
  (and (symbolp place)
       (not (eq :symbol-macro (variable-information place env)))))

(defmacro incf (place &optional (delta 1) &environment env)
  (if (non-macro-variable-symbol-p place env)
      `(setf ,place (+ ,place ,delta))
      (multiple-value-bind
       (tvars args vvars sform aform)
       (get-setf-method place env)
       (let ((vars (append tvars vvars))
	     (vals (append args (list `(+ ,aform ,delta)))))
	 `(let* ,(mapcar #'list vars vals) ,sform)))))

(defmacro decf (place &optional (delta 1) &environment env)
  (if (non-macro-variable-symbol-p place env)
      `(setf ,place (- ,place ,delta))
      (multiple-value-bind
       (tvars args vvars sform aform)
       (get-setf-method place env)
       (let ((vars (append tvars vvars))
	     (vals (append args (list `(- ,aform ,delta)))))
	 `(let* ,(mapcar #'list vars vals) ,sform)))))

(defmacro push (val place &environment env)
  (if (non-macro-variable-symbol-p place env)
      `(setq ,place (cons ,val ,place))
      (multiple-value-bind
       (tvars args vvars sform aform)
       (get-setf-method place env)
       (let ((vars (append tvars vvars))
	     (vals (append args (list `(cons ,val ,aform)))))
	 `(let* ,(mapcar #'list vars vals) ,sform)))))
    
(defmacro pop (place &environment env)
  (if (non-macro-variable-symbol-p place env)
      `(prog1 (first ,place) (setq ,place (rest ,place)))
      (multiple-value-bind
       (tvars args vvars sform aform)
       (get-setf-method place env)
       (let ((vars (append tvars vvars))
	     (vals (append args (list `(rest ,aform)))))
	 `(let* ,(mapcar #'list vars vals)
	    (prog1 (first ,aform) ,sform))))))

(defmacro pushnew (val place &rest rest &environment env)
  (if (non-macro-variable-symbol-p place env)
      `(setq ,place (adjoin ,val ,place ,@rest))
      (multiple-value-bind
       (tvars args vvars sform aform)
       (get-setf-method place env)
       (let ((vars (append tvars vvars))
	     (vals (append args (list `(adjoin ,val ,aform ,@rest)))))
	 `(let* ,(mapcar #'list vars vals) ,sform)))))

(defun rem-f (list y)
  (do* ((last nil x)
	(x list (cdr x)))
       ((null x) (values list nil))
       (when (eq y (first x))
	     (cond
	      (last
	       (rplacd last (cddr x))
	       (return (values list t)))
	      (t (return (values (cddr list) t)))))))

(defmacro remf (place indicator &environment env)
  (if (non-macro-variable-symbol-p place env)
      `(setq ,place (rem-f ,place ,indicator))
      (multiple-value-bind
       (tvars args vvars sform aform)
       (get-setf-method place env)
       (let ((vars (append tvars vvars))
	     (vals (append args (list `(rem-f ,aform ,indicator)))))
	 `(let* ,(mapcar #'list vars vals) ,sform)))))

;; This version does not optimize out the case where some places are
;; symbols, but it is not the most used macro in the world, and the
;; compiler should take care of these optimizations anyway.
(defmacro rotatef (&rest places &environment env)
  (let* ((smethods (mapcar #'(lambda (x)
			       (multiple-value-list (get-setf-method x env)))
			   places))
	 (tvars (apply #'append (mapcar #'first smethods)))
	 (args (apply #'append (mapcar #'second smethods)))
	 (vvars (apply #'append (mapcar #'third smethods)))
	 (sforms (mapcar #'fourth smethods))
	 (aforms (mapcar #'fifth smethods))
	 (rotaforms (append (rest aforms) (list (first aforms))))
	 (vars (append tvars vvars))
	 (vals (append args rotaforms)))
    `(let* ,(mapcar #'list vars vals) ,@sforms nil)))


;;;;;;
;;;;;; Replacements for Some Internal Special Forms
;;;;;;

(export '(psetq psetf))

;;;;
;;;; Replacements for internal PSETQ and PSETF
;;;;

(defun expand-pset-form (name pairs)
  (let ((gsyms nil)
	(syms nil)
	(vals nil))
    (loop
     (if (null pairs) (return))
     (push (gensym) gsyms)
     (push (first pairs) syms)
     (push (second pairs) vals)
     (setf pairs (rest (rest pairs))))
    (setf gsyms (reverse gsyms))
    (setf syms (reverse syms))
    (setf vals (reverse vals))
    (let ((bds (mapcar #'list gsyms vals))
	  (vsets (mapcar #'(lambda (x y) `(,name ,x ,y)) syms gsyms)))
      `(let* ,bds ,@vsets nil))))

(defmacro psetq (&rest pairs) (expand-pset-form 'setq pairs))
(defmacro psetf (&rest pairs) (expand-pset-form 'setf pairs))


;;;;;;
;;;;;;               Macro Definitions for Special Forms
;;;;;;
;;;;;;
;;;;;; There is one non-standard special forms, ERRSET. In addition,
;;;;;; the compiler treats CASE and NTH-VALUE as special forms. Code
;;;;;; walkers will need to handle these three cases separately.
;;;;;; ERRSET cannot be macroexpanded, and providing macro definitions
;;;;;; for NTH-VALUE and CASE would result in poor compiled
;;;;;; code. Perhaps they should CASE and NTH-VALUE should be handled
;;;;;; with compiler macros.

;;;;
;;;; DEFUN and DEFMACRO
;;;;

(define-special-form-macro defun (f args &rest body)
  (let* ((db (split-declarations body))
	 (decls (first db))
	 (b (second db))
	 (doc (third db))
	 (fexpr `#'(lambda ,args ,@decls (block ,f ,@b))))
    `(progn
       ,@(if doc `((%set-get ',f 'function-documentation ,doc)))
       (install-function ',f ,fexpr))))

(define-special-form-macro defmacro (f args &rest body)
  (multiple-value-bind (fexpr doc)
		       (make-macro-lambda-expression f args body)
    `(progn
       (eval-when (:compile-toplevel)
		  (push (cons ',f (coerce-to-macro ,fexpr)) *cmp-macros*))
       ,@(if doc `((%set-get ',f 'function-documentation ,doc)))
       (install-function ',f (coerce-to-macro ,fexpr)))))


;;;;
;;;; DEFMETH and DEFPROTO
;;;;

#+xlisp-stat
(define-special-form-macro defmeth (ob sym args &rest body)
  (let* ((db (split-declarations body))
	 (decls (first db))
	 (b (second db))
	 (doc (third db))
	 (fexpr `#'(lambda ,(cons 'self args) ,@decls (block ,sym ,@b))))
    `(progn
       (xlisp::add-method ,ob ',sym ,fexpr ,doc)
       ',sym)))

#+xlisp-stat
(define-special-form-macro defproto (name &optional ivars cvars parents doc)
  (let ((pp (gensym))
	(p (gensym))
	(d (gensym)))
    `(let* ((,pp ,parents)
	    (,p (apply #'make-object (if (listp ,pp) ,pp (list ,pp))))
	    (,d ,doc))
       (dolist (s ,cvars) (send ,p :add-slot s))
       (send ,p :make-prototype ',name ,ivars)
       (if ,d (send ,p :documentation 'proto ,d))
       (set ',name ,p)
       ',name)))


;;;;
;;;; DEFVAR, DEFPARAMETER and DEFCONSTANT
;;;;

(define-special-form-macro defvar (var &optional (value nil have-val) doc)
  `(progn 
     (eval-when (:compile-toplevel)
		(pushnew ',var *cmp-specials*))
     (mark-as-special ',var)
     ,@(if have-val `((unless (boundp ',var) (set ',var ,value))))
     ,@(if doc `((%set-get ',var 'variable-documentation ,doc)))
     ',var))

(define-special-form-macro defparameter (var value &optional doc)
  `(progn
     (eval-when (:compile-toplevel)
		(pushnew ',var *cmp-specials*))
     (mark-as-special ',var)
     (set ',var ,value)
     ,@(if doc `((%set-get ',var 'variable-documentation ,doc)))
     ',var))

;;**** is this the best way?
(define-special-form-macro defconstant (var value &optional doc)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
	      (mark-as-special ',var t ,value)
	      ,@(if doc `((%set-get ',var 'variable-documentation ,doc)))
	      ',var))


;;;;
;;;; AND, OR, UNLESS, WHEN, and COND Macros
;;;;

(defun make-progn-form (body)
  (if (consp body) (if (consp (rest body)) `(progn ,@body) (first body))))

(defun make-tagbody-body (body)
  (if (find-if-not #'consp body) `((tagbody ,@body)) `(,@body nil)))

(define-special-form-macro and (&rest args)
  (labels ((expand (args)
	     (if args
		 (let ((a (first args))
		       (rest (rest args)))
		   (if rest `(if ,a ,(expand rest)) a))
	         t)))
    (expand args)))

(define-special-form-macro or (&rest args)
  (labels ((expand (args)
             (if args
		 (let ((a (first args))
		       (rest (rest args)))
		   (if rest
		       (let ((s (gensym)))
			 `(let ((,s ,a))
			    (if ,s ,s ,(expand rest))))
		       a))
	         nil)))
    (expand args)))

(define-special-form-macro unless (test &rest body)
  `(if ,test nil ,(make-progn-form body)))

(define-special-form-macro when (test &rest body)
  `(if ,test ,(make-progn-form body)))


(define-special-form-macro cond (&rest clauses)
  (labels ((expand (clauses)
	     (if clauses
		 (let* ((fc (first clauses))
			(rc (rest clauses))
			(test (first fc))
			(consequents (rest fc)))
		   (if (eq test t)
		       (if consequents (make-progn-form consequents) t)
		       (if consequents
			   `(if ,test
				,(make-progn-form consequents)
			        ,(expand rc))
			   (let ((tsym (gensym "T")))
			     `(let ((,tsym ,test))
				(if ,tsym ,tsym ,(expand rc))))))))))
    (expand clauses)))


;;;;
;;;; RETURN Macro
;;;;

(define-special-form-macro return (&optional val) `(return-from nil ,val))


;;;;
;;;; PROG, PROG*, PROG1, and PROG2 Macros
;;;;

(define-special-form-macro prog (vlist &rest body)
  (let ((db (split-declarations body)))
    `(block nil (let ,vlist ,@(first db) ,@(make-tagbody-body (second db))))))

(define-special-form-macro prog* (vlist &rest body)
  (let ((db (split-declarations body)))
    `(block nil (let* ,vlist ,@(first db) ,@(make-tagbody-body (second db))))))

(define-special-form-macro prog1 (first &rest rest)
  (let ((s (gensym)))
    `(let ((,s ,first)) ,@rest ,s)))

(define-special-form-macro prog2 (first second &rest rest)
  (let ((s (gensym)))
    `(progn ,first (let ((,s ,second)) ,@rest ,s))))


;;;;
;;;; LOOP, DOTIMES, DOLIST, DO, and DO* Macros
;;;;

(define-special-form-macro loop (&rest body)
    (let ((start (gensym "LOOP")))
      `(block nil (tagbody ,start (progn ,@body) (go ,start)))))

(defun do-loop-binding-variables (bds)
  (mapcar #'(lambda (x) (if (consp x) (first x) x)) bds))

(defun do-loop-binding-values (bds)
  (mapcar #'(lambda (x) (if (consp x) (second x) nil)) bds))

(defun do-loop-binding-steps (bds)
  (mapcar #'(lambda (x) (if (consp x) (rest (rest x)) nil)) bds))

(defun make-do-step-pairs (vars steps)
  (apply #'append
	 (delete nil (mapcar #'(lambda (x y) (if y (cons x y))) vars steps))))

;; This definition duplicates the test form (it is used at the start
;; of the loop and in the body, so the code may be a little longer
;; than it could be. But it is now possible to define dotimes and
;; dolist in terms of do without loss of efficiency. Also this version
;; removes the need for a jump label bewteen the update and test
;; forms. It may therefore be possible to reduce the updte-branch
;; instructions to a single instruction in the peephole optimization
;; phase.
(defun make-do-loop (letsym setsym bds tr body)
  (let* ((vars (do-loop-binding-variables bds))
	 (ivals (do-loop-binding-values bds))
	 (slist (make-do-step-pairs vars (do-loop-binding-steps bds)))
	 (test (first tr))
	 (result (rest tr))
	 (db (split-declarations body))
	 (loop-sym (gensym "LOOP"))
	 (return-sym (gensym "RETURN")))
    `(block nil
	    (,letsym ,(mapcar #'list vars ivals)
	      ,@(first db)
	      (tagbody
	       (when ,test (go ,return-sym))
	       ,loop-sym
	       ,@(second db)
	       (,setsym ,@slist)
	       (unless ,test (go ,loop-sym))
	       ,return-sym
	       (return-from nil ,(make-progn-form result)))))))

(define-special-form-macro do (bds tr &rest body)
  (make-do-loop 'let 'psetq bds tr body))

(define-special-form-macro do* (bds tr &rest body)
  (make-do-loop 'let* 'setq bds tr body))

(define-special-form-macro dotimes ((isym nval &optional res) &rest body)
  (let ((nsym (gensym "N")))
    `(do ((,isym 0 (1+ ,isym))
	  (,nsym ,nval))
	 ((not (< ,isym ,nsym)) ,res)
	 ,@body)))

(define-special-form-macro dolist ((esym lval &optional res) &rest body)
  (let ((lsym (gensym "LIST")))
    `(do* ((,lsym ,lval (cdr ,lsym))
	   (,esym (car ,lsym) (car ,lsym)))
	  ((not (consp ,lsym)) ,res)
	  ,@body)))


;;;;
;;;; IN-PACKAGE Macro
;;;;

(define-special-form-macro in-package (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
	      (let ((newpack (find-package ',name)))
		(unless newpack (error "can't find package ~s" ',name))
		(setf *package* newpack))))


;;;;
;;;; TIME Macro
;;;;

(define-special-form-macro time (expr)
  (let ((rtsym (gensym "RUN"))
	(gtsym (gensym "GC")))
    `(let ((,rtsym (get-internal-run-time))
	   (,gtsym(get-internal-gc-time)))
       (multiple-value-prog1
	,expr
	(format t "The evaluation took ~,2f seconds; ~,2f seconds in GC.~%"
		(float (/ (- (get-internal-run-time) ,rtsym)
			  internal-time-units-per-second))
		(float (/ (- (get-internal-gc-time) ,gtsym)
			  internal-time-units-per-second)))))))


;;;;
;;;; TRACE and UNTRACE Macros
;;;;

(define-special-form-macro trace (&rest args)
  `(dolist (s ',args *tracelist*)
     (unless (symbolp s) (error "not a symbol - ~s" s))
     (pushnew s *tracelist*)))

(define-special-form-macro untrace (&rest args)
  (if args
      `(dolist (s ',args *tracelist*)
         (unless (symbolp s) (error "not a symbol - ~s" s))
	 (setf *tracelist* (delete s *tracelist*)))
      `(setf *tracelist* nil)))


;;;;
;;;; WITHOUT-INTERRUPTS Macro
;;;;

(export 'system::without-interrupts "SYSTEM")
(defmacro system::without-interrupts (&rest body)
  `(progn
     (system::enable-interrupts nil)
     (unwind-protect
	 (progn ,@body)
       (system::enable-interrupts t))))


;;;;
;;;; This is the final file in the common module
;;;;

(provide "common")
