;;;;
;;;; Additional Common Lisp Functions for XLISP-STAT 2.0
;;;; XLISP-STAT 2.1 Copyright (c) 1990-95, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;

(in-package "XLISP")

;;;;;;
;;;;;;                   New DEFSTRUCT System
;;;;;;            Replaces the internal special form
;;;;;;
;;;;;;
;;;;;; Limitations: Error checking is poor.
;;;;;;              Multiple :constructor options are not allowed.
;;;;;;              Typed structures do not support :initial-offset's.
;;;;;;              The :type slot option is ignored.
;;;;;;              Probably lots more.

;;;;
;;;; Some Compiler Support Functions
;;;;

(defvar *cmp-structs*)

(defun cmp-get-slotinfo (structname)
  (if (boundp '*cmp-structs*)
      (assoc structname *cmp-structs*)))

(defun cmp-register-slotinfo (structname slotinfo)
  (if (boundp '*cmp-structs*)
      (push (cons structname slotinfo) *cmp-structs*)))


;;;;
;;;; Some Runtime Support Functions
;;;;

(defun get-structure-slot-default (type slot)
  (second (assoc slot (get type '*struct-slots*))))

(defun set-structure-slot-default (type slot new)
  (let* ((slotinfo (get type '*struct-slots*))
	 (entry (assoc slot slotinfo)))
    (when entry
	  (let ((new-entry (copy-list entry)))
	    (setf (second new-entry) new)
	    (setf (get type '*struct-slots*)
		  (subst new-entry entry slotinfo))))))

#|
;;**** This hash table based method may be slightly better for large
;;**** structures
(let ((default-table (make-hash-table :test 'equal))
      (lookup-cell (cons nil nil)))

  (defun set-structure-slot-default (type slot new)
    (setf (gethash (cons type slot) default-table) new))

  (defun get-structure-slot-default (type slot)
    (setf (car lookup-cell) type)
    (setf (cdr lookup-cell) slot)
    (gethash lookup-cell default-table)))
|#

(defun default-structure-slot-value (type slot)
  (let ((init (get-structure-slot-default type slot)))
    (if init (funcall init))))

(defun install-sharp-s-constructor (structname f)
  (if (symbolp f)
      (setf (get structname '*struct-constructor*) f)
      (let* ((symname (concatenate 'string "MAKE-" (symbol-name structname)))
	     (sym (make-symbol symname)))
	(setf (get structname '*struct-constructor*) sym)
	(setf (symbol-function sym) f))))

(defun install-structure-slots (structname include slots)
  (let* ((parent (first include))
	 (parent-info (if parent (get (first include) '*struct-slots*)))
	 (slotinfo (append parent-info slots))
	 (overrides (rest include)))
    (setf (get structname '*struct-slots*) slotinfo)
    (dolist (s slots)
      (set-structure-slot-default structname (first s) (second s)))
    (when parent
	  (dolist (i parent-info)
	    (let* ((name (structure-slotinfo-name i))
		   (default (get-structure-slot-default parent name)))
	      (set-structure-slot-default structname name default)))
	  (dolist (new overrides)
	    (set-structure-slot-default structname (first new)
					(second new))))))


;;;;
;;;; Slot Info Representation
;;;;

(defun make-structure-slotinfo (name form readonly) (list name form readonly))
(defun structure-slotinfo-name (x) (first x))
(defun structure-slotinfo-form (x) (second x))
(defun structure-slotinfo-read-only (x) (third x))


;;;;
;;;; Slot Name Comparison Function
;;;;

(defun structure-slot-eql (x y) (string= (symbol-name x) (symbol-name y)))


;;;;
;;;; Slot Option Extractors
;;;;

(defun convert-structure-slot-options (slots)
  (mapcar #'(lambda (x)
	      (if (consp x)
		  (make-structure-slotinfo (first x)
					   (second x)
					   (getf (rest (rest x)) :read-only))
		  (make-structure-slotinfo x nil nil)))
	  slots))

(defun get-structure-parent-slotinfo (p)
  (let ((si (get p '*struct-slots* 'none)))
    (if (eq si 'none)
	(let ((cmpinfo (cmp-get-slotinfo p)))
	  (unless cmpinfo (error "no slot info available for structure ~s" p))
	  (copy-list (cdr cmpinfo)))
        si)))

(defun get-structure-slotinfo (include slots)
  (let ((parent (first include)))
    (append (if parent (get-structure-parent-slotinfo parent)) slots)))


;;;;
;;;; Slot Option Expanders
;;;;

(defun check-structure-slots (structspec slotspecs)
  (let* ((structname (if (consp structspec) (first structspec) structspec))
	 (options (if (consp structspec) (rest structspec)))
	 (include (get-structure-include options))
	 (parent (first include))
	 (overrides (rest include))
	 (owninfo (convert-structure-slot-options slotspecs))
	 (incinfo (if parent (get-structure-parent-slotinfo parent)))
	 (info (append incinfo owninfo)))
    (flet ((same (x y)
		 (structure-slot-eql (structure-slotinfo-name x)
				     (structure-slotinfo-name y))))
      ;; check include slot options for existenc and consistent read-only state
      (dolist (new overrides)
	(let ((old (find new incinfo :test #'same)))
	  (unless old
		  (error "no inherited slot named ~s"
			 (symbol-name (structure-slotinfo-name new))))
	  (when (and (structure-slotinfo-read-only old)
		     (not (structure-slotinfo-read-only new)))
		(error "inherited slot ~s must be read-only"
		       (structure-slotinfo-name new)))))
      ;; check slots for uniqueness
      (dolist (own owninfo)
        (when (< 1 (count own info :test #'same))
	      (error "only one slot named ~s allowed"
		     (symbol-name (structure-slotinfo-name own))))))))

(defun make-structure-slot-forms (structname include slots)
  (flet ((fix-info (x)
	  (let ((name (structure-slotinfo-name x))
		(form (structure-slotinfo-form x))
		(readonly (structure-slotinfo-read-only x)))
	    `(list ',name ,(if form `#'(lambda () ,form)) ,readonly))))
    (let ((incname (first include))
	  (incslots (mapcar #'fix-info (rest include)))
	  (ownslots (mapcar #'fix-info slots)))
      `(install-structure-slots ',structname
				,(if incname `(list ',incname ,@incslots))
				,(if ownslots `(list ,@ownslots))))))
	  
(defun make-structure-slot-accessor-forms (conc-name slotinfo typed)
  (let* ((forms nil)
	 (named (rest typed))
	 (i (if (and typed (not named)) 0 1))
	 (ref-fun (if typed 'elt '%struct-ref)))
    (dolist (sk slotinfo)
      (let* ((sn (structure-slotinfo-name sk))
	     (name (intern (concatenate 'string conc-name (symbol-name sn))))
	     (ro (structure-slotinfo-read-only sk)))
	(push `(defun ,name (x) (,ref-fun x ,i)) forms)
	;;**** change this to inlining later?
	(push `(define-compiler-macro ,name (x) (list ',ref-fun x ,i)) forms)
	(push (if ro
		  `(defsetf ,name (x) (v) (error "slot ~s is read-only" ',sn))
		  `(defsetf ,name (x) (v)
		     ,(if typed
			  `(list 'setf (list 'elt x ,i) v)
			  `(list '%struct-set x ,i v))))
	      forms))
      (incf i))
    (if forms `(progn ,@(nreverse forms)))))


;;;;
;;;; Structure Option Extractors
;;;;

(defconstant *structure-options*
  '(:conc-name :copier :constructor :include :named
	       :print-function :predicate :type))

(defun check-structure-specification (structspec)
  (let ((structname (if (consp structspec) (first structspec) structspec))
	(options (if (consp structspec) (rest structspec))))
    (unless (symbolp structname) (error "bad structure name - ~s" structname))
    (flet ((check (x s) (when x (error "~a - ~s" s x)))
	   (is-opt (x) (or (eq x :named) (consp x)))
	   (optname (x) (if (symbolp x) x (first x))))
      (check (find-if-not #'is-opt options) "bad structure option")
      (check (find-if-not #'(lambda (x) (member x *structure-options*))
			  options
			  :key #'optname)
	     "unknown structure option")
      (dolist (opt *structure-options*)
        (check (if (< 1 (count opt options :key #'optname)) opt)
	       "structure option used more than once")))))

(defun find-structure-option (name options)
  (find name options :key #'(lambda (x) (if (symbolp x) x (first x)))))

(defun get-structure-option-symbol (name options optname s1 s2)
  (let ((option (find-structure-option optname options)))
    (if option
	(let ((sym (second option)))
	  (unless (symbolp sym) (error "~s is not a symbol"))
	  sym)
        (intern (concatenate 'string s1 (string name) s2)))))

(defun get-structure-conc-name (structname options)
  (let ((option (find-structure-option :conc-name options)))
    (if option
	(let ((name (second option)))
	  (if name (string name) ""))
        (concatenate 'string (symbol-name structname) "-"))))

(defun get-structure-copier (structname options)
  (get-structure-option-symbol structname options :copier "COPY-" ""))

(defun get-structure-constructor (structname options)
  (let ((option (find-structure-option :constructor options)))
    (if option
	(cond
	 ((null (second option)) nil)
	 ((consp (rest (rest option))) (list (second option) (third option)))
	 (t (second option)))
        (intern (concatenate 'string "MAKE-" (symbol-name structname))))))

(defun get-structure-include (options)
  (let ((option (find-structure-option :include options)))
    (when option
	  (cons (second option)
		(convert-structure-slot-options (rest (rest option)))))))

(defun get-structure-predicate (structname options)
  (get-structure-option-symbol structname options :predicate "" "-P"))

(defun get-structure-print-function (options)
  (second (find-structure-option :print-function options)))

(defun get-structure-type (options)
  (let ((type (second (find-structure-option :type options))))
    (when type (cons type (find-structure-option :named options)))))


;;;;
;;;; Structure Option Expanders
;;;;

(defun make-structure-copier-form (copier)
  (when copier `(defun ,copier (x) (%copy-struct x))))

(defun make-structure-predicate-form (structname predicate type)
  (when (and predicate (not type))
	`(progn
	   (defun ,predicate (x) (%struct-type-p ',structname x))
	   (define-compiler-macro ,predicate (x)
	     (list '%struct-type-p '',structname x)))))

(defun make-structure-print-function-form (structname printfun type)
  (if (and printfun (not type))
      `(setf (get ',structname '*struct-print-function*)
	     ,(if (symbolp printfun)
		  (list 'quote printfun)
		printfun))
    `(remprop ',structname '*struct-print-function*)))
    

(defun make-structure-constructor-form-body (structname slotnames tn)
  (let ((type (first tn))
	(named (rest tn)))
    (cond
     ((eq type 'list) `(list ,@(if named `(',structname)) ,@slotnames))
     ((eq type 'vector) `(vector ,@(if named `(',structname)) ,@slotnames))
     ((and (consp type) (eq (first type) 'vector))
      (let* ((slen (length slotnames))
	     (n (if named (+ slen 1) slen))
	     (args (if named `(',structname ,@slotnames) slotnames))
	     (etype (second type)))
	`(make-array ,n
		     :element-type ',etype
		     :initial-contents (list ,@args))))
     (t `(%make-struct ',structname ,@slotnames)))))

(defun make-standard-structure-constructor-form (structname slotinfo tn)
  (let ((alist nil)
	(slotnames (mapcar #'structure-slotinfo-name slotinfo)))
    (dolist (s slotnames)
      (push `(,s (default-structure-slot-value ',structname ',s)) alist))
    (when alist (setf alist `(&key ,@(nreverse alist))))
    `(,alist
      ,(make-structure-constructor-form-body structname slotnames tn))))

(defun fixup-structure-constructor-argform (name a)
  (flet ((new-form (a) `(,a (default-structure-slot-value ',name ',a))))
    (cond
     ((symbolp a) (new-form a))
     ((and (consp a) (null (rest a)))
      (let* ((syment (first a))
	     (sym (if (symbolp syment) syment (second syment))))
	(new-form sym)))
     (t a))))

(defun remove-structure-constructor-slot (a slots)
  (cond
   ((symbolp a) (remove a slots))
   ((symbolp (first a)) (remove (first a) slots))
   ((consp (first a)) (remove (second (first a)) slots))
   (t slots)))

(defun structure-constructor-arglist (name alist slots)
  (let ((new-alist nil)
	(key nil))
    (dolist (a alist)
      (cond
       ((member a lambda-list-keywords) (setf key a))
       (t
	(when (member key '(&optional &key))
	      (setf a (fixup-structure-constructor-argform name a)))))
      (setf slots (remove-structure-constructor-slot a slots))
      (push a new-alist))
    (when slots
	  (pushnew '&aux new-alist)
	  (dolist (s slots)
	    (push (fixup-structure-constructor-argform name s) new-alist)))
    (nreverse new-alist)))

(defun make-boa-structure-constructor-form (structname slotinfo alist tn)
  (let* ((slots (mapcar #'structure-slotinfo-name slotinfo))
	 (args (structure-constructor-arglist structname alist slots)))
    `(,args
      ,(make-structure-constructor-form-body structname slots tn))))

(defun make-structure-constructor-form (structname slotinfo constructor tn)
  (cond
   ((symbolp constructor)
    `(defun ,constructor
       ,@(make-standard-structure-constructor-form structname slotinfo tn)))
   ((consp constructor)
    `(defun ,(first constructor)
       ,@(make-boa-structure-constructor-form structname
					      slotinfo
					      (second
					       constructor) tn)))))

(defun make-sharp-s-structure-constructor-form (structname slotinfo tn)
  `(install-sharp-s-constructor 
    ',structname
    #'(lambda
	,@(make-standard-structure-constructor-form structname slotinfo tn))))

(defun make-structure-include-form (structname include)
  (when include
	`(setf (get ',structname '*struct-include*) ',(first include))))


;;;;
;;;; DEFSTRUCT Macro
;;;;

(defmacro defstruct (structspec &rest slotspecs)
  (check-structure-specification structspec)
  ;;**** drop doc string for now
  (when (stringp (first slotspecs)) (pop slotspecs))
  (check-structure-slots structspec slotspecs)
  (let* ((structname (if (consp structspec) (first structspec) structspec))
	 (options (if (consp structspec) (rest structspec)))
	 (slots (convert-structure-slot-options slotspecs))
	 (conc-name (get-structure-conc-name structname options))
	 (copier (get-structure-copier structname options))
	 (constructor (get-structure-constructor structname options))
	 (include (get-structure-include options))
	 (printfun (get-structure-print-function options))
	 (predicate (get-structure-predicate structname options))
	 (type (get-structure-type options))
	 (slotinfo (get-structure-slotinfo include slots)))
    (flet ((list-if (x) (if x (list x))))
      `(progn
	 (eval-when (:compile-toplevel)
		    (cmp-register-slotinfo ',structname ',slotinfo))
	 ,(make-structure-slot-forms structname include slots)
	 ,@(list-if (make-structure-slot-accessor-forms conc-name
							slotinfo
							type))
	 ,@(list-if (make-structure-copier-form copier))
	 ,@(list-if (make-structure-predicate-form structname predicate type))
	 ,@(list-if (make-structure-print-function-form structname
							printfun
							type))
	 ,@(list-if (make-structure-constructor-form structname
						     slotinfo
						     constructor
						     type))
	 ,(make-sharp-s-structure-constructor-form structname slotinfo type)
	 ,@(list-if (make-structure-include-form structname include))
	 ',structname))))
