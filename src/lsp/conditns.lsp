;;;;
;;;; Common Lisp Condition System for XLISP-STAT 2.0
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;


;; The condition system is used if the variable xlisp::*condition-hook* is
;; not nil. The internal functions xlerror and xlcerror, as well as the 
;; Lisp-callable C functions xerror, xcerror, xsignal, xwarn, xbreak and
;; xdebug, call the hook with the function symbol, the frame index and the
;; current environment as arguments. The environment can be used by evalhook
;; for evaluating things in the environment that was active when the
;; condition was signaled.
;;
;; If an image containing the condition system is saved, then the function
;; xlisp::use-conditions needs t be called on startup to enable the
;; condition system.

;; To do:
;; **** rethink handling of internal xlabort's
;; **** make full version of assert, etypecase, ctypecase, ecase, ccase
;; **** make remaining standard condition types

;; **** collect debugger features
;; **** different debugging tools -- get-value, set-value as alt to evalhook?
;; **** move around in stack frames?


(in-package "XLISP")

(require "common")


;;;;
;;;; Exported Symbols
;;;;

;; Signaling Conditions
(export '(error cerror signal *break-on-signals*))

;; Assertions
(export '(check-type assert))

;; Exhaustive Case Analysis
(export '(etypecase ctypecase ecase ccase))

;; Handling Conditions
(export '(handler-case ignore-errors handler-bind))

;; Defining and Creating Conditions
(export '(define-condition make-condition))

;; Establishing Restarts
(export '(with-simple-restart restart-case restart-bind))

;; Finding and manipulating Restarts
(export '(compute-restarts restart-name find-restart invoke-restart
	  invoke-restart-interactively))

;; Warnings
(export 'warn)

;; Restart Functions
(export '(abort continue muffle-warning store-value use-value))

;; Debugging Utilities
(export '(break invoke-debugger *debugger-hook*))

;; Predefined Condition Types
(export '(condition simple-condition serious-condition
	  error simple-error arithmetic-error division-by-zero
	  cell-error unbound-variable undefined-function
	  control-error file-error package-error program-error
	  stream-error end-of-file type-error simple-type-error
	  storage-condition warning simple-warning

	  simple-condition-format-string simple-condition-format-arguments
	  type-error-datum type-error-expected-type package-error-package
	  stream-error-stream file-error-pathname cell-error-name
	  arithmetic-error-operation arithmetic-error-operands))

;; Restart Data Type
(export 'restart)


;;;;
;;;; Public Variables
;;;;

(defvar *break-on-signals* nil)
(defvar *debugger-hook* nil)


;;;;
;;;; Internal Variables
;;;;


;; Unique Markers
(defvar *eof-mark* (gensym "EOF"))
(defvar *not-found* (gensym "NOT-FOUND"))

;; Handler Variables
(defvar *default-handler* nil)
(defvar *active-handlers* nil)

;; Restart Variables
(defvar *default-restart* nil)
(defvar *active-restarts* nil)
(defvar *continue-restarts* nil)
(defvar *condition-restarts* nil)

;; Debugger Variables
(defvar *debug-level* 0)
(defvar *debug-env* nil)
(defvar *debug-frame* nil)
(defvar *debug-print-length* nil)
(defvar *debug-print-level* nil)


;;;;
;;;; Initialization Function
;;;; (Must be called on each system startup, e. g. as a startup action)
;;;;

(defun use-conditions (&optional (reset nil))
  (setf *condition-hook* 'condition-hook)
  (setf *active-restarts* (list (list nil *default-restart*)))
  (setf *continue-restarts* *active-restarts*)
  (setf *active-handlers*
	(if *default-handler* (list (list nil t *default-handler*))))
  (if reset (top-level)))

(defun unuse-conditions ()
  (setf *condition-hook* nil)
  (top-level))


;;;;
;;;; Internal Restart Representation
;;;;

(defstruct (restart (:print-function print-restart))
  name
  function
  (test-function #'(lambda (c) (declare (ignore c)) t))
  interactive-function
  report-function)

(defun print-restart (restart stream depth)
  (declare (ignore depth))
  (if *print-escape*
      (format stream "#<Restart ~a: ~d>"
	      (restart-name restart)
	      (address-of restart))
    (let ((report (restart-report-function restart)))
      (if report
	  (funcall report stream)
	(let ((name (restart-name restart)))
	  (if name
	      (format stream "~a." name)
	    (error "can't print restart ~s without escapes"
		   restart)))))))

(setf *default-restart*
      (make-restart :name 'abort
		    :function
		      #'(lambda (&rest args)
			  (declare (ignore args))
			  (top-level nil))
		    :test-function #'(lambda (c) (declare (ignore c)) t)
		    :report-function
		      #'(lambda (s)
			  (format s "Return to Lisp Toplevel."))))


;;;;
;;;; Helper Functions for RESTART-CASE and RESTART-BIND
;;;;

(defun push-restarts (c reslist)
  (dolist (r (reverse reslist))
    (push (list c r) *active-restarts*)))

(defun expand-restart-binding-form (x)
  (let ((name (first x))
	(function (second x))
	(options (rest (rest x))))
    (unless function (error "restart form missing function - ~s" x))
    (when (and (null name) (not (getf options :report-function)))
	  (error "anonymous restart needs a report function - ~s"x))
    `(make-restart :name ',name :function ,function ,@options)))

(defun make-restart-case-parts (tagsym argsym cases)
  (let ((syms (mapcar #'(lambda (x) (gensym "GO")) cases))
	(head nil)
	(tail nil))
    (mapc #'(lambda (c s)
	      (push `(,(first c)
		      #'(lambda (&rest temp)
			  (setq ,argsym temp)
			  (go ,s))
		      ,@(transform-restart-case-options c))
		    head)
	      (push s tail)
	      (push `(return-from ,tagsym
				  (apply #'(lambda ,(second c)
					     ,@(restart-case-case-body c))
					 ,argsym))
		    tail))
	  cases
	  syms)
    (cons (nreverse head) (nreverse tail))))

(defun transform-restart-case-options (c)
  (let ((opts nil))
    (loop
     (setf c (rest (rest c)))
     (case (first c)
       (:report
	(push :report-function opts)
	(push (if (stringp (second c))
		  `(function (lambda (s) (format s "~a" ,(second c))))
		  `(function ,(second c)))
	      opts))
       (:test
	(push :test-function opts)
	(push `(function ,(second c)) opts))
       (:interactive
	(push :interactive-function opts)
	(push `(function ,(second c)) opts))
       (t (return (nreverse opts)))))))

(defun restart-case-case-body (c)
  (loop
   (setf c (rest (rest c)))
   (unless (member (first c) '(:report :test :interactive))
	   (return c))))

(defun condition-restarts (expr clist)
  (if (and (consp expr) (member (first expr) '(error cerror signal warn)))
      (mapcar #'second
	      (butlast *active-restarts*
		       (- (length *active-restarts*) (length clist))))))


;;;;
;;;; Internal Condition Representation
;;;;

(setf (get 'condition '*struct-slots*) nil)
(setf (get 'condition '*struct-print-function*) 'print-condition)

(defun transform-condition-report-option (x)
  (if x
      (if (stringp x) x `(function ,x))))

(defun plist-to-alist (plist)
  (do ((plist plist (rest (rest plist)))
       (alist nil (push (list (first plist) (second plist)) alist)))
      ((not (consp (rest plist))) (nreverse alist))))

(defun transform-condition-slot-options (spec)
  (let* ((name (if (consp spec) (first spec) spec))
	 (opts (if (consp spec) (plist-to-alist (rest spec)) nil))
	 (iform (assoc :initform opts)))
    (if iform
	(let ((form (second iform)))
	  `(cons
	    ',name
	    (cons
	     ,(if (constantp form)
		       `'(:initform ,form)
		       `(list :initform
			      (list 'eval
				    (list (function (lambda () ,form))))))
	     ',(remove-if #'(lambda (x) (eq (first x) :initform)) opts))))
        `'(,name ,@opts))))

(defun print-condition (c s d)
  (declare (ignore d))
  (let ((type (type-of c)))
    (if *print-escape*
	(format s "#<Condition ~s: ~d>" (type-of c) (address-of c))
        (let ((rep (get type '*condition-report*)))
	  (cond
	   ((null rep) (format s "~s" type))
	   ((stringp rep) (format s "~s" rep))
	   (t (funcall rep c s)))))))

(defun make-condition-class (name parent report doc slots)
  (setf (get name '*struct-print-function*) 'print-condition)
  (if parent (setf (get name '*struct-include*) parent))
  (setf (get name '*condition-report*)
	(if report report (get parent '*condition-report*)))
  (if (stringp doc) (setf (documentation name 'type) doc))
  (let ((old (if parent (mapcar #'copy-list (get parent '*struct-slots*))))
	(new nil))
    (dolist (s slots)
      (unless (assoc (first s) old) (push (list (first s) nil nil) new)))
    (let ((entries (append old (nreverse new))))
      (dolist (spec slots)
        (let* ((entry (assoc (first spec) entries))
	       (opts (rest spec))
	       (i (+ (position entry entries) 1)))
	  (dolist (opt opts)
	    (case (first opt)
	     (:reader
	      (setf (symbol-function (second opt))
		    (eval `(function (lambda (x) (%struct-ref x ,i))))))
	     (:writer
	      (setf (symbol-function (second opt))
		    (eval `(function (lambda (x v) (%struct-set x ,i v))))))
	     (:accessor
	      (setf (symbol-function (second opt))
		    (eval `(function (lambda (x) (%struct-ref x ,i)))))
	      (setf (get (second opt) '*setf*)
		    (eval `(function (lambda (x v) (%struct-set x ,i v))))))
	     (:initarg (push (second opt) (second entry)))
	     (:initform (setf (third entry) (second opt)))
	    ))))
      (setf (get name '*struct-slots*) entries)))
  name)

(defun initialize-condition (c info args)
  (do* ((i 1 (+ i 1))
	(info info (rest info))
	(vi (first info) (first info)))
      ((null info))
      (let* ((iargs (second vi))
	     (ival  *not-found*)
	     (iform (third vi)))
	(dolist (i iargs)
	  (setf ival (getf args i *not-found*))
	  (unless (eq ival *not-found*) (return)))
	(if (eq ival *not-found*)
	    (setf ival (if (constantp iform) iform (eval iform))))
	(%struct-set c i ival))))


;;;;
;;;; Helper Functions for HANDLER-BIND and HANDLER-CASE
;;;;      

(defun reverse-expand-handler-bind-forms (th)
  (let ((forms nil))
    (dolist (f th forms)
      (push `(list ',(first f) ,(second f)) forms))))

(defun push-condition-handlers (clist active)
  (dolist (ch clist)
    (push (cons active ch) *active-handlers*)))

(defun handler-case-handler-bind-forms (hforms varsym tagsyms)
  (mapcar #'(lambda (ht ts)
	      `(,(first ht) #'(lambda (temp) (setq ,varsym temp) (go ,ts))))
	  hforms
	  tagsyms))

(defun expand-handler-case-bodies (hforms bsym varsym tagsyms)
  (apply #'nconc
	 (mapcar #'(lambda (hf ts)
		     (let ((v (if (second hf) (first (second hf)) varsym)))
		       `(,ts
			 (return-from ,bsym
				      (let ((,v ,varsym))
					,@(rest (rest hf)))))))
		 hforms
		 tagsyms)))


;;;;
;;;; Hook Function and Lisp-Level Signaling Functions
;;;;

(defun handle-condition (c)
  (if (typep c *break-on-signals*)
      (with-simple-restart (continue "Proceed with signalling.")
        (format *debug-io* "~&Break on signal: ")
	(do-debugger c)))
  (dolist (he *active-handlers*)
    (let* ((*active-handlers* (first he))
	   (tspec (second he))
	   (h (third he)))
      (if (typep c tspec) (funcall h c)))))

(defun condition-argument (datum args &optional
				 (simple-type 'simple-error)
				 (type 'condition))
  (cond
   ((typep datum type) datum) ;;**** check for no additional args?
   ((symbolp datum) (apply #'make-condition datum args))
   ((stringp datum)
    (make-condition simple-type :format-string datum :format-arguments args))
   (t (error "bad condition arguments - ~s" (cons datum args)))))

(defun base-condition-hook (type *debug-frame* *debug-env* &rest args)
  (let ((*condition-hook* 'condition-hook))
    (case type
      (error (apply #'do-error args))
      (cerror (apply #'do-cerror args))
      (signal (apply #'do-signal args))
      (warn (apply #'do-warn args))
      (break (apply #'do-break args))
      (debug (apply #'do-debugger args)))))

(defun condition-hook (&rest args)
  (let ((*condition-hook* 'condition-hook))
    (handler-bind
     ((unbound-variable #'(lambda (c)
                      (autoload-variable (cell-error-name c))))
      (undefined-function #'(lambda (c)
                        (autoload-function (cell-error-name c)))))
     (apply #'base-condition-hook args))))

(defun do-error (datum &rest args)
  (let ((condition (condition-argument datum args)))
    (with-condition-restarts condition *condition-restarts*
      (setf *condition-restarts* nil)
      (handle-condition condition)
      (format *debug-io* "~&Error: ")
      (do-debugger condition))))

(defun do-cerror (cmsg datum &rest args)
  (let ((condition (condition-argument datum args)))
    (with-condition-restarts condition *condition-restarts*
      (setf *condition-restarts* nil)
      (restart-case
       (progn
	 (handle-condition condition)
	 (format *debug-io* "~&Error: ")
	 (do-debugger condition))
       (continue ()
	:report (lambda (s) (apply #'format s cmsg args))
	:test (lambda (c) (eq c condition)))))
      nil))

(defun do-signal (datum &rest args)
  (let ((condition (condition-argument datum args 'simple-condition)))
    (with-condition-restarts condition *condition-restarts*
      (setf *condition-restarts* nil)
      (handle-condition condition))
    nil))

(defun do-warn (datum &rest args)
  (let ((condition (condition-argument datum args 'simple-warning 'warning)))
    (with-condition-restarts condition *condition-restarts*
      (setf *condition-restarts* nil)
      (restart-case
       (progn
	 (signal condition)
	 (format *error-output* "~&Warning: ~a~%" condition))
       (muffle-warning ()
        :report "Muffle warning"
	:test (lambda (c) (eq c condition)))))
    nil))

(defun do-break (&optional (fmt-string "**BREAK**") &rest fmt-args)
  (with-simple-restart (continue "Return from BREAK.")
    (format *debug-io* "Break: ")
    (do-debugger
     (make-condition 'simple-condition
		     :format-string fmt-string
		     :format-arguments fmt-args)))
  nil)


;;;;
;;;; Debugger Functions
;;;;

(defun do-debugger (condition)
  ;; should probably check for a condition
  (let ((*print-readably* nil))
    (when *debugger-hook*
      (let* ((hook *debugger-hook*)
	     (*debugger-hook* nil))
	(funcall hook condition hook)))
    (let* ((*debug-level* (+ *debug-level* 1))
	   (current-level *debug-level*)
	   (*print-level* (if *debug-print-level*
			      *debug-print-level*
			    *print-level*))
	   (*print-length* (if *debug-print-length*
			       *debug-print-length*
			     *print-length*)))
      ;; print the error message
      (when condition
	(multiple-value-bind (val err)
			     (ignore-errors
			      (format *debug-io* "~a~%" condition))
	  (declare (ignore val))
	  (when err (format *debug-io* "~s~%" condition))))

      ;; flush the input buffer and reset the system internals
      (reset-system)

      ;; do the back trace
      (if *tracenable* (baktrace (if *tracelimit* *tracelimit* -1)))

      ;; read-eval-print loop
      (let ((*continue-restarts* (compute-restarts condition)))
	(loop
	 (with-simple-restart (abort "Return to break level ~d." current-level)

	   (when *batch-mode* (format *debug-io* "uncaught error~%") (exit))

	   ;; print restart information (**** optional??)
	   (format *debug-io* "Break level ~d.~%" current-level)
	   (format
	    *debug-io*
	    "To continue, type (continue n), where n is an option number:~%")
	   (dotimes (i (length *continue-restarts*))
	     (multiple-value-bind
	      (val err)
	      (ignore-errors
	       (format *debug-io* "~2d: ~a~%" i (nth i *continue-restarts*)))
	      (declare (ignore val))
	      (when err 
		(ignore-errors
		 (format *debug-io* "~s~%" (nth i *continue-restarts*))))))

	   (loop
	    ;; print a prompt
	    (if (eq *package* (find-package "USER"))
		(format *debug-io* "~&~d> " *debug-level*)
	      (format *debug-io* "~&~A ~d> "
		      (package-name *package*)
		      *debug-level*))

	    ;; read and save an input expression
	    (let ((expr (read *debug-io* nil *eof-mark*)))
	      (if (eq expr *eof-mark*) (continue 0));;**** is this right??
	      (setf +++ ++ ++ + + - - expr))

	    ;; evaluate the expression, save and print the results
	    (let ((vals (multiple-value-list (evalhook - nil nil *debug-env*))))
	      (setf *** ** ** * * (first vals))

	      (fresh-line *debug-io*)

	      (dolist (v vals) (format *debug-io* "~s~%" v))))))))))

(defun debug-fun ()
  (if (or (null *debug-frame*) (null (stack-value *debug-frame*)))
      nil
      (stack-value (+ *debug-frame* 1))))

(defun clean-up (&optional c) (continue c))

(defun baktrace (&optional levels (print-args *baktrace-print-arguments*))
  (if *debug-frame*
      (do ((fp *debug-frame* (- fp (stack-value fp)))
	   (n (if levels levels -1) (- n 1)))
	  ((or (= n 0) (null (stack-value fp))))
	  (let ((p (+ fp 1)))
	    (format *error-output* "Function: ~s~%" (stack-value p))
	    (incf p)
	    (if print-args
		(let ((argc (stack-value p)))
		  (incf p)
		  (when (> argc 0)
			(format *error-output* "Arguments:~%")
			(dotimes (i argc)
			  (format *error-output* "  ~s~%"
				  (stack-value (+ p i))))))))))
  (values))

(defun show-bindings (&optional vars)
  (dolist (a (first *debug-env*))
    (if (consp a)
	(dolist (b a)
	  (if (consp b)
	      (let ((s (car b))
		    (v (cdr b)))
		(if (and (symbolp s)
			 (or (null vars)
			     (eq s vars)
			     (and (consp vars) (member s vars))))
		    (format *error-output* "~s~15t~s~%" s v)))))))
  (values))

(defmacro get-value (form) `(evalhook ,form nil nil *debug-env*))

(defmacro set-value (form val) `(evalhook (setf ,form ,val) *debug-env*))


;;;;
;;;; Public Interface
;;;;

(defun prompt-for (type fmt-string &rest fmt-args)
  (loop
   (apply #'format *debug-io* fmt-string fmt-args)
   (let ((val (eval (read *debug-io*))))
     (if (typep val type) (return val))
     (format *debug-io* "~s is not of type ~s.~%" val type))))
       
;;**** simple version -- use this in define-cmp-macro
(defun type-check (x spec)
  (unless (typep x spec) (error "~s is not of type ~s." x spec))
  nil)

#|
(defmacro check-type (place spec &optional string)
  `(type-check ,place ',spec))
|#

;; version of check-type that returns the final value of the place form
(defmacro base-check-type (place spec &optional string)
  (let ((valsym (gensym "VAL")))
    `(loop
      (let ((,valsym ,place))
	(if (typep ,valsym ',spec)
	    (return ,valsym)
	    (restart-case
	     (error 'check-type-error
		    :datum ,valsym
		    :expected-type ',spec
		    :form ',place
		    :type-string ,string)
	     (store-value (,valsym)
	      :report "Store new value."
	      :interactive (lambda ()
			     (list
			      (prompt-for ',spec "Value for ~s: " ',place)))
	      (setf ,place ,valsym))))))))

(defmacro check-type (place spec &optional string)
  `(progn (base-check-type ,place ,spec ,string)
	  nil))
     
;;**** simple versions
(defmacro assert (testform &optional places datum &rest args)
  (if datum
      `(unless ,testform (error ,datum ,@args))
      `(unless ,testform
	       (error "The assertion ~S failed" ',testform))))

(defmacro etypecase (var &rest forms)
  (let ((vsym (gensym "VAR")))
    `(let ((,vsym ,var))
       (typecase ,vsym
		 ,@forms
		 (t (error 'type-error
			   :datum ,vsym
			   :expected-type '(or ,@(mapcar #'first forms))))))))

(defmacro ctypecase (var &rest body)
  `(typecase (base-check-type ,var (or ,@(mapcar #'first body)))
	     ,@body))

(defun compute-case-match-type (cases)
  (let ((keys nil))
    (dolist (b cases (cons 'member (nreverse keys)))
      (if (consp (first b))
	  (dolist (k (first b)) (push k keys)))
	  (push (first b) keys))))

(defmacro ecase (var &rest forms)
  (let ((vsym (gensym "VAR")))
    `(let ((,vsym ,var))
       (case ,vsym
	     ,@forms
	     (t (error 'type-error
		       :datum ,vsym
		       :expected-type (compute-case-match-type ',forms)))))))

(defmacro ccase (var &rest body)
  `(case (base-check-type ,var ,(compute-case-match-type body))
	 ,@body))

(defmacro handler-case (expr &rest hforms)
  (if (eq (first (first (last hforms))) :no-error)
      (let ((errsym (gensym "ERROR"))
	    (normsym (gensym "NORMAL"))
	    (ne-form (first (last hforms)))
	    (e-forms (butlast hforms)))
	`(block ,errsym
	   (multiple-value-call #'(lambda ,@(rest ne-form))
	     (block ,normsym
	       (return-from ,errsym
	         (handler-case (return-from ,normsym ,expr)
		   ,@e-forms))))))
      (let ((bsym (gensym "BLOCK"))
	    (varsym (gensym "VAR"))
	    (tagsyms (mapcar #'(lambda (x) (gensym "TAG")) hforms)))
	`(block ,bsym
	   (let (,varsym)
	     (tagbody
	      (handler-bind
	       ,(handler-case-handler-bind-forms hforms varsym tagsyms)
	       (return-from ,bsym ,expr))
	      ,@(expand-handler-case-bodies hforms bsym varsym tagsyms)))))))

(defmacro ignore-errors (&rest forms)
  `(handler-case (progn ,@forms)
     (error (c) (values nil c))))

(defmacro handler-bind (th &rest body)
  (let ((valsym (gensym "VALUE")))  
    `(let ((*active-handlers* *active-handlers*))
       (push-condition-handlers (list ,@(reverse-expand-handler-bind-forms th))
				*active-handlers*)
       ;; this errset traps internal xlabort's (from stack overflows)
       ;; and converts them to calls to (abort)
       (let ((,valsym (errset (multiple-value-list (progn ,@body)) nil)))
	 (if ,valsym
	     (values-list (first ,valsym))
	     (error "stack overflow"))))))

#|
(defmacro handler-bind (th &rest body)
  `(let ((*active-handlers* *active-handlers*))
     (push-condition-handlers (list ,@(reverse-expand-handler-bind-forms th))
			      *active-handlers*)
     ,@body))
|#

(defmacro handler-bind (th &rest body)
  (let ((valsym (gensym "VALUE")))  
    `(let ((*active-handlers* *active-handlers*))
       (push-condition-handlers (list ,@(reverse-expand-handler-bind-forms th))
				*active-handlers*)
       ;; this errset traps internal xlabort's (from stack overflows)
       ;; and converts them to calls to (abort)
       (let ((,valsym (errset (multiple-value-list (progn ,@body)))))
	 (if ,valsym
	     (values-list (first ,valsym))
	     (error "stack overflow"))))))

(defmacro define-condition (name plist &optional slots &rest options)
  (if (< 1 (length plist)) (error "multiple inheritance not supported"))
  (if (null plist) (error "new conditions must inherit from an existing one"))
  `(make-condition-class ',name
			 ',(first plist)
			 ,(transform-condition-report-option
			   (second (assoc :report options)))
			 ',(second (assoc :documentation options))
			 (list ,@(mapcar #'transform-condition-slot-options
					 slots))))

(defun make-condition (type &rest args)
  (when (eq (get type '*struct-slots* *not-found*) *not-found*)
	(error "bad condition type - ~s" type))
  (let* ((info (get type '*struct-slots*))
	 (c (apply #'%make-struct type (make-list (length info)))))
    (initialize-condition c info args)
    c))

(defmacro with-simple-restart (rfa &rest forms)
  (let ((restart-name (first rfa))
	(format-string (second rfa))
	(format-args (rest (rest rfa))))
    `(restart-case (progn ,@forms)
       (,restart-name ()
         :report (lambda (stream) (format stream ,format-string ,@format-args))
	 (values nil t)))))

(defmacro restart-case (expr &rest cases)
  (let* ((tagsym (gensym "TAG"))
	 (argsym (gensym "ARGS"))
	 (valsym (gensym "VALS"))
	 (parts (make-restart-case-parts tagsym argsym cases)))
    `(block ,tagsym
       (let ((,argsym nil))
	 (tagbody
	  (restart-bind
	   ,(first parts)
	   (let ((*condition-restarts* (condition-restarts ',expr ',cases)))
	     (return-from ,tagsym ,expr)))
	  ,@(rest parts))))))

#|
(defmacro restart-bind (bds &rest body)
  (let ((valsym (gensym "VALUE")))
    `(let ((*active-restarts* *active-restarts*))
       (push-restarts nil (list ,@(mapcar #'expand-restart-binding-form bds)))
       ;; this errset traps internal xlabort's (from stack overflows)
       ;; and converts them to calls to (abort)
       (let ((,valsym (errset (multiple-value-list (progn ,@body)))))
	 (if ,valsym (values-list (first ,valsym)) (abort))))))
|#

(defmacro restart-bind (bds &rest body)
  `(let ((*active-restarts* *active-restarts*))
     (push-restarts nil (list ,@(mapcar #'expand-restart-binding-form bds)))
     ,@body))

(defmacro with-condition-restarts (condition rlist &rest forms)
  `(let ((*active-restarts* *active-restarts*))
     (push-restarts ,condition ,rlist)
     ,@forms))


(defun compute-restarts (&optional condition)
  (let ((result nil))
    (dolist (cr *active-restarts*)
      (if (restart-entry-applicable-p cr condition)
	  (push (second cr) result)))
    (nreverse (delete-duplicates result))))
	
;restart-name
(defun restart-entry-applicable-p (cr condition)
  (let ((c (first cr))
	(r (second cr)))
    (if c
	(eq c condition)
        (or (null condition)
	    (funcall (restart-test-function r) condition)))))
  
(defun find-restart (identifier &optional condition)
  (cond
   ((null identifier) nil)
   ((symbolp identifier)
    (second
     (find-if #'(lambda (x)
		  (and (restart-entry-applicable-p x condition)
		       (eq identifier (restart-name (second x)))))
	      *active-restarts*)))
   ((restart-p identifier)
    (second (find identifier *active-restarts* :key #'second)))))
   
	       
(defun invoke-restart (identifier &rest args)
  (let ((restart (find-restart identifier)))
    (if restart
	(apply (restart-function restart) args)
        (error "invalid restart - ~s" identifier))))

(defun invoke-restart-interactively (identifier)
  (let* ((restart (find-restart identifier))
	 (ifun (restart-interactive-function restart))
	 (rfun (restart-function restart)))
    (if restart
	(if ifun (apply rfun (funcall ifun)) (funcall rfun))
        (error "invalid restart - ~s" identifier))))


(defun abort (&optional condition)
  (invoke-restart (find-restart 'abort condition)))

(defun continue (&optional condition)
  (if (integerp condition)
      (let ((restart (nth condition *continue-restarts*)))
	(if restart (invoke-restart-interactively restart)))
      (let ((restart (find-restart 'continue condition)))
	(if restart (invoke-restart restart)))))

(defun muffle-warning (&optional condition)
  (invoke-restart (find-restart 'muffle-warning condition)))

(defun store-value (value &optional condition)
  (let ((restart (find-restart 'store-value condition)))
    (if restart (invoke-restart restart value))))

(defun use-value (value &optional condition)
  (let ((restart (find-restart 'use-value condition)))
    (if restart (invoke-restart restart value))))


;;;;
;;;; Condition Types
;;;;

(defun print-simple-condition (c s)
  (if *print-escape*
      (print-condition c s nil)
      (apply #'format
	     s
	     (simple-condition-format-string c)
	     (simple-condition-format-arguments c))))

(define-condition simple-condition (condition)
  ((format-string :accessor simple-condition-format-string
		  :initform "Simple condition."
		  :initarg :format-string)
   (format-arguments :accessor simple-condition-format-arguments
		     :initarg :format-arguments))
  (:report print-simple-condition))

(define-condition serious-condition (condition))
(define-condition error (serious-condition))

(define-condition simple-error (error)
  ((format-string :initform "Simple error." :initarg :format-string)
   (format-arguments :initarg :format-arguments))
  (:report print-simple-condition))

(define-condition warning (condition))

(define-condition simple-warning (warning)
  ((format-string :initform "Simple warning." :initarg :format-string)
   (format-arguments :initarg :format-arguments))
  (:report print-simple-condition))

(define-condition storage-condition (condition))

(define-condition cell-error (error)
  ((name :accessor cell-error-name :initarg :name))
  (:report "Cell error"))

(define-condition unbound-variable (cell-error)
  ()
  (:report
   (lambda (c s)
     (format s "The variable ~s is unbound." (cell-error-name c)))))

(define-condition undefined-function (cell-error)
  ()
  (:report
   (lambda (c s)
     (format s "The function ~s is not defined." (cell-error-name c)))))

(define-condition type-error (error)
  (format-string
   format-arguments
   (datum :initarg :datum :accessor type-error-datum)
   (expected-type :initarg :expected-type
		  :accessor type-error-expected-type))
  (:report
   (lambda (c s)
     (format s "~s is not of type ~s."
	     (type-error-datum c)
	     (type-error-expected-type c)))))

(define-condition check-type-error (type-error)
  ((form :initarg :form :accessor check-type-error-form)
   (type-string :initarg :type-string :accessor check-type-error-type-string))
  (:report
   (lambda (c s)
     (if (check-type-error-type-string c)
	 (format s "The value of ~s, ~s, is not ~a."
		 (check-type-error-form c)
		 (type-error-datum c)
		 (check-type-error-type-string c))
         (format s "The value of ~s, ~s, is not of type ~s."
		 (check-type-error-form c)
		 (type-error-datum c)
		 (type-error-expected-type c))))))

(define-condition simple-type-error (type-error))

#|
			arithmetic-error
				division-by-zero
			control-error
			file-error
			package-error
			program-error
			stream-error
				end-of-file

package-error-package

stream-error-stream
file-error-pathname

arithmetic-error-operation
arithmetic-error-operands
|#
