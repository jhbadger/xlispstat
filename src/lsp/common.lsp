;;;;
;;;; Additional Common Lisp Functions for XLISP-STAT 2.0
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;

(in-package "XLISP")

;**** This hack is necessary for the moment because X is one of the
;**** plot symbols. This should be dropped one the statistics stuff is
;**** properly assigned to packages.
(export 'x)

(export '(fifth sixth seventh eighth ninth tenth))

(defun fifth (x) (nth 4 x))
(defun sixth (x) (nth 5 x))
(defun seventh (x) (nth 6 x))
(defun eighth (x) (nth 7 x))
(defun ninth (x) (nth 8 x))
(defun tenth (x) (nth 9 x))


;;;;
;;;; Compiler symbols
;;;;

(export '(compile compile-file))

(defvar *cmp-setf*)

;;;;
;;;; Other symbols
;;;;

(export 'step)


;;;;
;;;; Simplified DEFTYPE and some additional types
;;;;

(export '(deftype flonum short-float single-float double-float long-float real
	  sequence))

(defun deftype-arglist-fix (args)
  (let ((optpos (position '&optional args)))
    (if optpos
	(let* ((start (+ optpos 1))
	       (nkey (position-if #'(lambda (x)
				      (member x lambda-list-keywords))
				args
				:start start))
	       (end (if nkey nkey (length args)))
	       (nargs (copy-list args)))
	  (do ((i start (+ i 1)))
	      ((<= end i) nargs)
	      (let ((v (nth i nargs)))
		(if (symbolp v) (setf (nth i nargs) `(,v '*))))))
	args)))

(defmacro deftype (type args &rest forms)
  (let ((fargs (deftype-arglist-fix args)))
    `(progn (setf (get ',type '*type-spec*) #'(lambda ,fargs ,@forms))
	    ',type)))

(deftype flonum () 'float)
(deftype short-float () 'float)
(deftype single-float () 'float)
(deftype double-float () 'float)
(deftype long-float () 'float)

(deftype real () '(or integer ratio float))
(deftype rational () '(or integer ratio))

;;**** keyword type?

(deftype sequence () '(satisfies sequencep))


;;;;
;;;; EVAL-WHEN
;;;;

(export '(eval-when))

(defmacro eval-when (when &rest body)
  (if (or (member 'eval when) (member :execute when))
      `(progn ,@body)))


;;;;
;;;; Defsetf and documentation functions
;;;; Corrected for Common Lisp compatibility (requires XLISP-PLUS 2.1e)
;;;;  Modified by Tom Almy, 7/92
;;;;  Corrected again in 6/93
;;;;  and again (Luke Tierney) 11/93
;;;;

(export '(defsetf documentation))

(defun apply-arg-rotate (f args) 
  (apply f (car (last args)) (butlast args)))

; (defsetf) - define setf method
(defmacro defsetf (sym first &rest rest)
  (if (symbolp first)
      `(progn (setf (get ',sym '*setf*) ',first)
	      (remprop ',sym '*setf-lambda*)
	      (eval-when (:compile-toplevel)
			 (push (list ',sym '*setf* ',first) *cmp-setf*))
	      ',sym)
      (let* ((f `#'(lambda ,(append (car rest) first) ,@(cdr rest)))
	     (args (gensym))
	     (ff `#'(lambda (&rest ,args) (apply-arg-rotate ,f ,args))))
        `(progn
          (setf (get ',sym '*setf-lambda*) ; changed *setf* to *setf-lambda*
		,ff)
	  (eval-when (:compile-toplevel)
		     (push (list ',sym '*setf-lambda* ,ff) *cmp-setf*))
	  (remprop ',sym '*setf*)
          ',sym))))

;; (load-help) - read in file positions fo accessing help info.
(defun load-help ()
  (when 
   (and (null *help-loaded*) (streamp *help-stream*))
   (princ "loading in help file information - this will take a minute ...")
   (force-output)
   (setq *help-loaded* t)
   (file-position *help-stream* 0)
   (do ((item (read *help-stream* nil '*eof*) 
              (read *help-stream* nil '*eof*))) 
       ((eq item '*eof*))
       (cond
	((and item
	      (symbolp item) 
	      (null (get item 'function-documentation)))
	 (setf (get item 'function-documentation) 
	       (file-position *help-stream*)))
	((consp item)
	 (case (cadr item)
	       (function (if (null (get (car item) 'function-documentation))
			     (setf (get (car item) 'function-documentation) 
				   (file-position *help-stream*))))
	       (variable (if (null (get (car item) 'variable-documentation))
			     (setf (get (car item) 'variable-documentation) 
				   (file-position *help-stream*))))
	       (type (if (null (get (car item) 'type-documentation))
			 (setf (get (car item) 'type-documentation) 
			       (file-position *help-stream*))))
	       (object
		(if (and (boundp (car item))
			 (objectp (symbol-value (car item)))
			 (null (send (symbol-value (car item))
				     :internal-doc
				     (caddr item))))
		    (send (symbol-value (car item))
			  :documentation
			  (caddr item)
			  (file-position *help-stream*))))))))
   (princ "done")
   (terpri)))

(defun documentation (symbol doc-type)
  (load-help)
  (let ((doc  (case doc-type
                (function (get symbol 'function-documentation))
                (variable (get symbol 'variable-documentation))
                (type     (get symbol 'type-documentation))
                (setf     (get symbol 'setf-documentation)))))
    (when (and (numberp doc) (streamp *help-stream*))
          (file-position *help-stream* doc)
          (setq doc (read *help-stream*)))
    (if (stringp doc) doc)))

(defsetf documentation (symbol doc-type) (value)
  `(case ,doc-type
     (function (setf (get ,symbol 'function-documentation) ,value))
     (variable (setf (get ,symbol 'variable-documentation) ,value))
     (type     (setf (get ,symbol 'type-documentation) ,value))
     (setf     (setf (get ,symbol 'setf-documentation) ,value))))

(defsetf first (x) (v) `(setf (car ,x) ,v))  
(defsetf second (x) (v) `(setf (nth 1 ,x) ,v))
(defsetf third (x) (v) `(setf (nth 2 ,x) ,v))
(defsetf fourth (x) (v) `(setf (nth 3 ,x) ,v))
(defsetf fifth (x) (v) `(setf (nth 4 ,x) ,v))
(defsetf sixth (x) (v) `(setf (nth 5 ,x) ,v))
(defsetf seventh (x) (v) `(setf (nth 6 ,x) ,v))
(defsetf eighth (x) (v) `(setf (nth 7 ,x) ,v))
(defsetf ninth (x) (v) `(setf (nth 8 ,x) ,v))
(defsetf tenth (x) (v) `(setf (nth 9 ,x) ,v))

(defsetf rest (x) (v) `(setf (cdr ,x) ,v))

(defsetf %struct-ref (x i) (v) `(%struct-set ,x ,i ,v))

(defun %set-rm-aref (x i v) (setf (aref (compound-data-seq x) i) v))
(defsetf row-major-aref %set-rm-aref)

;; Defsetf's for forms handled by the internal version.  These need to
;; appear before any redefinition of setf for bootstrapping purposes.
(defsetf get %set-get)
(defsetf symbol-value %set-symbol-value)
(defsetf symbol-function %set-symbol-function)
(defsetf symbol-plist %set-symbol-plist)
(defsetf car %set-car)
(defsetf cdr %set-cdr)
(defsetf aref %set-aref)
(defsetf gethash %set-gethash)

(defsetf select set-select)
(defsetf slot-value slot-value)

(defsetf nth %set-nth)
(defsetf elt %set-elt)
(defsetf svref %set-svref)


;;;;
;;;;
;;;; Modules, provide and require
;;;;
;;;;

(export '(*modules* provide require probe-file))

(defvar *modules* nil)
    
(defun provide (name)
  (pushnew name *modules* :test #'equal))
  
(export '(system::*module-path* system::create-module-path)
        "SYSTEM")

(defvar *module-path* nil)

(defun require (name &optional (path (string name)))
  (let ((name (string name))
        (pathlist (if (listp path) path (list path))))
    (unless (member name *modules* :test #'equal)
            (dolist (pathname pathlist)
              (let ((rpath (find-require-file pathname)))
                (if rpath
                    (load rpath)
                  (load pathname :if-does-not-exist nil)))))))

(defun find-require-file (path)
  (let ((type (pathname-type path)))
    (dolist (dir *module-path*)
      (let ((p (merge-pathnames path dir)))
        (cond
         ((eq (system::file-type p) :directory)
          (let* ((dl (append (pathname-directory p) (list (pathname-name p))))
                 (d (make-pathname :directory dl
                                   :device (pathname-device p)
                                   :host (pathname-host p)))
                 (ap (merge-pathnames "_autoidx" d)))
            (when (or (probe-file (merge-pathnames ap ".lsp"))
                      (probe-file (merge-pathnames ap ".fsl")))
                  (return ap))))
         (type (when (probe-file p) (return p)))
         ((or (probe-file (merge-pathnames p ".lsp"))
              (probe-file (merge-pathnames p ".fsl")))
          (return p))
         ((probe-file p) (return p)))))))

(defun create-module-path ()
  (list (make-pathname :directory '(:relative))
        *default-path*
        (merge-pathnames (make-pathname :directory '(:relative "Examples"))
                         *default-path*)))

(defun probe-file (f)
  (when (open f :direction :probe)
	(truename f)))


;;;;
;;;;
;;;; Miscellaneous Functions
;;;;
;;;;

(export '(matrixp equalp y-or-n-p yes-or-no-p
	  incf decf push pop pushnew remf rotatef
	  with-input-from-string with-output-to-string
	  with-open-file with-open-stream read-from-string
	  realp typecase))

(defun matrixp (x) (and (arrayp x) (= (array-rank x) 2)))

; equalp rewritten by Tom Almy to better match Common Lisp
(defun equalp (x y)
  (cond ((equal x y) t)
	((numberp x) (if (numberp y) (= x y) nil))
	((characterp x) (if (characterp y) (char-equal x y) nil))
	((listp x)
	 (and (listp y) (= (length x) (length y)) (every #'equalp x y)))
	((and (or (arrayp x) (stringp x)) 
	      (or (arrayp y) (stringp y))
	      (eql (length x) (length y)))
	 (every #'equalp x y))))

; Modified by TAA
(defun y-or-n-p (&rest args)
       (reset-system)
       (when args (fresh-line) (apply #'format *terminal-io* args))
       (do ((answer (string-trim " " (read-line))
		    (string-trim " " (read-line))))
	   ((or (string-equal answer "Y")
		(string-equal answer "N"))
	    (string-equal answer "Y"))
	   (princ " Answer \"y\" or \"n\": " *terminal-io*)))

; Based on y-or-n-p
(defun yes-or-no-p (&rest args)
       (reset-system)
       (when args (fresh-line) (apply #'format *terminal-io* args))
       (do ((answer (string-trim " " (read-line))
		    (string-trim " " (read-line))))
	   ((or (string-equal answer "YES")
		(string-equal answer "NO"))
	    (string-equal answer "YES"))
	   (princ " Answer \"yes\" or \"no\": " *terminal-io*)))

;***** modified version with keywords?
(defmacro with-input-from-string (stream-string &rest body)
  (let ((stream (first stream-string))
	(string (second stream-string))
	(start (second (member :start (cddr stream-string))))
	(end (second (member :end (cddr stream-string))))
	(index (second (member :index (cddr stream-string)))))
    (when (null start) (setf start 0))
    (if index
	(let ((str (gensym)))
	  `(let* ((,str ,string)
		  (,stream (make-string-input-stream ,str ,start ,end)))
	     (prog1 (progn ,@body)
	       (setf ,index 
		     (- (length ,str)
			(length (get-output-stream-list ,stream)))))))
        `(let ((,stream (make-string-input-stream ,string ,start ,end)))
	   (progn ,@body)))))

(defmacro with-output-to-string (str-list &rest body)
  (let ((stream (first str-list)))
    `(let ((,stream (make-string-output-stream)))
       (progn ,@body)
       (get-output-stream-string ,stream))))

(defmacro with-open-file (stream-file-args &rest body)
  (let ((stream (first stream-file-args))
	(file-args (rest stream-file-args)))
    `(let ((,stream (open ,@file-args)))
       (unwind-protect 
	   (progn ,@body)
	 (when ,stream (close ,stream))))))

(defmacro with-open-stream (stream-args &rest body)
  `(let ((,(first stream-args) ,(second stream-args)))
     (unwind-protect
	 (progn ,@body)
       (when ,(first stream-args) (close ,(first stream-args))))))

(defun read-from-string (string &optional (eof-error-p t) eof-value
				&key (start 0) end preserve-whitespace)
  (read (make-string-input-stream string start end) eof-error-p eof-value))

(defun realp (x) (or (rationalp x) (floatp x)))

(defmacro typecase (x &rest forms)
  (let ((varsym (gensym "VAR")))
    (flet ((fix-clause (f) (cons `(typep ,varsym ',(first f)) (rest f))))
      `(let ((,varsym ,x))
	 (cond
	  ,@(if (member (first (first (last forms))) '(t otherwise))
		(append (mapcar #'fix-clause (butlast forms))
			(list (cons t (rest (first (last forms))))))
	        (mapcar #'fix-clause forms)))))))

;;
;; from xlisp-2.1f
;;

(export '(pairlis copy-alist copy-tree signum))

;; pairlis does not check for lengths of keys and values being unequal
(defun pairlis (keys values &optional list)
  (nconc (mapcar #'cons keys values) list))

(defun copy-alist (list)
  (if (null list)
      'nil
      (cons (if (consp (car list))
		(cons (caar list) (cdar list))
	        (car list))
	    (copy-alist (cdr list)))))

(defun copy-tree (list)
  (if (consp list)
      (cons (copy-tree (car list)) (copy-tree (cdr list)))
      list))

(defun signum (x)
  (if (numberp x)
      (if (zerop x) x (/ x (abs x)))
      (map-elements #'signum x)))

;; Hyperbolic functions    Ken Whedbee  from CLtL

(export '(logtest cis sinh cosh tanh asinh acosh atanh))

(defun logtest (x y) (not (zerop (logand x y))))

(defun cis (x) (exp (* #c(0.0 1.0) x)))

(defun sinh (x) (/ (- (exp x) (exp (- x))) 2.0))
(defun cosh (x) (/ (+ (exp x) (exp (- x))) 2.0))
(defun tanh (x) (/ (sinh x) (cosh x)))

(defun asinh (x) (log (+ x (sqrt (+ 1.0 (* x x))))))
(defun acosh (x) (log (+ x (* (1+ x) (sqrt (/ (1- x) (1+ x)))))))
(defun atanh (x) (log (/ (1+ x) (sqrt (- 1.0 (* x x))))))
    
;; array functions.   KCW    from  Kyoto Common Lisp

(export '(fill acons))

(defun fill (sequence item &key (start 0) end)
  (when (null end) (setf end (length sequence)))
  (do ((i start (1+ i)))
      ((>= i end) sequence)
      (setf (elt sequence i) item)))

(defun acons (x y a)         ; from CLtL
  (cons (cons x y) a))


;; more set functions.  KCW    from Kyoto Common Lisp

;; Modified to pass keys to subfunctions without checking here
;; (more efficient)

;; (Tom Almy states:) we can't get the destructive versions of union
;; intersection, and set-difference to run faster than the non-destructive
;; subrs. Therefore we will just have the destructive versions do their
;; non-destructive counterparts

(export
 '(nunion nintersection nset-difference set-exclusive-or nset-exclusive-or))

(setf (symbol-function 'nunion) 
      (symbol-function 'union)
      (symbol-function 'nintersection) 
      (symbol-function 'intersection)
      (symbol-function 'nset-difference) 
      (symbol-function 'set-difference))

(defun set-exclusive-or (list1 list2 &rest rest)
  (append (apply #'set-difference list1 list2 rest)
          (apply #'set-difference list2 list1 rest)))

(defun nset-exclusive-or (list1 list2 &rest rest)
  (nconc (apply #'set-difference list1 list2 rest)
         (apply #'set-difference list2 list1 rest)))


;;;;
;;;;
;;;; Additional Common Lisp Functions for Xlisp 2.0
;;;; From the init.lsp file supplied in the Xlisp distribution
;;;;
;;;;

(export '(set-macro-character get-macro-character))

; (set-macro-character ch fun [ tflag [ rtab ] ])
(defun set-macro-character (ch fun &optional tflag (rtab *readtable*))
    (setf (aref rtab (char-int ch))
          (cons (if tflag :tmacro :nmacro) fun))
    t)

; (get-macro-character ch [ rtab ])
(defun get-macro-character (ch &optional (rtab *readtable*))
  (if (consp (aref rtab (char-int ch)))
    (cdr (aref *readtable* (char-int ch)))
    nil))

;;;;
;;;;
;;;; Additional Readtable Functions
;;;;
;;;;
;;**** need to be fixed once proper readtables are available

(export '(copy-readtable readtablep set-syntax-from-char))

(defconstant *common-lisp-readtable* (copy-array *readtable*))

(defun copy-readtable (&optional (rt *readtable*) (dest (make-array 256)))
  (let ((source (if rt rt *common-lisp-readtable*)))
    (replace dest source)))

(defun readtablep (rt) (and (vectorp rt) (= (length rt) 256)))

(defun set-syntax-from-char (to from &optional (dest *readtable*) src)
  (let ((source (if src src *common-lisp-readtable*)))
    (setf (aref dest (char-int to)) (aref source (char-int from)))
    t))


;;;;
;;;;
;;;; Additional System Functions for Xlisp 2.0
;;;; From the init.lsp file supplied in the Xlisp distribution
;;;;
;;;;

(export '(savefun debug nodebug))

; (savefun fun) - save a function definition to a file
(defmacro savefun (fun)
  `(let* ((fname (concatenate 'string (symbol-name ',fun) ".lsp"))
          (fval (function-lambda-expression (symbol-function ',fun)))
          (fp (open fname :direction :output)))
     (cond (fp (print (cons (if (eq (car fval) 'lambda)
                                'defun
                                'defmacro)
                            (cons ',fun (cdr fval))) fp)
               (close fp)
               fname)
           (t nil))))

; (debug) - enable debug breaks
(defun debug ()
  (setq *breakenable* t))

; (nodebug) - disable debug breaks
(defun nodebug ()
  (setq *breakenable* nil))


;;;;;
;;;;; Symbol and Package Functions
;;;;;

(export '(defpackage do-symbols do-external-symbols do-all-symbols
	  apropos apropos-list))

(defmacro do-symbol-arrays (s res a body)
  (let ((arraysym (gensym))
	(isym (gensym))
	(asym (gensym))
	(listsym (gensym)))	 
    `(let ((,arraysym ,a)
	   (,isym 0)
	   (,asym nil)
	   (,listsym nil)
	   (,s nil))
       (block nil
         (tagbody
	  new-array
	  (when (null ,arraysym)
		(setf ,s nil)
		(return ,res))
	  (setf ,asym (first ,arraysym) ,arraysym (rest ,arraysym) ,isym -1)
	  new-list
	  (setf ,isym (1+ ,isym))
	  (if (<= 199 ,isym) (go new-array))
	  (setf ,listsym (aref ,asym ,isym))
	  new-item
	  (if (null ,listsym) (go new-list))
	  (setf ,s (first ,listsym) ,listsym (rest ,listsym))
	  (tagbody ,@body)
	  (go new-item))))))

(defmacro do-symbols (spr &rest body)
  (let ((packsym (gensym))
	(usessym (gensym))
	(arraysym (gensym)))
    `(let* ((,packsym ,(if (second spr) (second spr) '*package*))
	    (,usessym (package-use-list ,packsym))
	    (,arraysym (cons (package-obarray ,packsym nil)
			     (mapcar #'package-obarray
				     (cons ,packsym ,usessym)))))
       (do-symbol-arrays ,(first spr) ,(third spr) ,arraysym ,body))))

(defmacro do-external-symbols (spr &rest body)
  (let ((packsym (gensym))
	(arraysym (gensym)))
    `(let* ((,packsym ,(if (second spr) (second spr) '*package*))
	    (,arraysym (list (package-obarray ,packsym))))
       (do-symbol-arrays ,(first spr) ,(third spr) ,arraysym ,body))))

(defmacro do-all-symbols (sr &rest body)
  (let ((packsym (gensym))
	(arraysym (gensym)))
    `(let* ((,packsym (list-all-packages))
	    (,arraysym nil))
       (dolist (p ,packsym)
         (push (package-obarray p) ,arraysym)
	 (push (package-obarray p nil) ,arraysym))
       (do-symbol-arrays ,(first sr) ,(second sr) ,arraysym ,body))))

#|
;;**** this overflows in compilation
(defmacro defpackage (pname &rest options)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let* ((pname ',pname)
	    (options ',options)
	    (pack (find-package ',pname))
	    (nicknames nil))
       (dolist (opt options)
         (if (eq (first opt) :nicknames)
	     (setf nicknames (append (rest opt) nicknames))))
       (if pack
	   (rename-package pack
			   pname
			   (mapcar #'string
				   (append nicknames
					   (package-nicknames pack))))
           (setf pack (make-package pname :nicknames 
				    (mapcar #'string nicknames))))
       (dolist (opt options)
         (case (first opt)
	   (:shadow (shadow (mapcar #'string (rest opt)) pack))
	   (:shadowing-import-from
	    (let ((from-pack (find-package (second opt))))
	      (dolist (sname (rest (rest opt)))
	        (multiple-value-bind (sym found)
			             (find-symbol (string sname) from-pack)
		  (if found
		      (shadowing-import sym pack)
		      (error "no symbol named ~s in package ~s"
			     (string sname)
			     from-pack))))))))
       (dolist (opt options)
         (if (eq (first opt) :use)
	     (use-package (mapcar #'string (rest opt)) pack)))
       (dolist (opt options)
         (case (first opt)
	   (:intern
	    (dolist (sname (rest opt)) (intern (string sname) pack)))
	   (:import-from
	    (let ((from-pack (find-package (second opt))))
	      (dolist (sname (rest (rest opt)))
	        (multiple-value-bind (sym found)
				     (find-symbol (string sname) from-pack)
	          (if found
		      (import sym pack)
		      (error "no symbol named ~s in package ~s"
			     (string sname)
			     from-pack))))))))
       (dolist (opt options)
         (if (eq (first opt) :export)
	     (dolist (sname (rest opt))
	       (export (intern (string sname) pack) pack))))
       pack)))
|#
(defmacro defpackage (pname &rest options)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
	      (do-defpackage ',pname ',options)))

(defun do-defpackage (pname options)
  (let ((pack (find-package pname))
	(nicknames nil))
    (dolist (opt options)
      (if (eq (first opt) :nicknames)
	  (setf nicknames (append (rest opt) nicknames))))
    (if pack
	(rename-package pack
			pname
			(remove-duplicates
			 (mapcar #'string
				 (append nicknames (package-nicknames pack)))
			 :test #'string=))
        (setf pack (make-package pname :nicknames 
				 (remove-duplicates
				  (mapcar #'string nicknames)
				  :test #'string=))))
    (dolist (opt options)
      (case (first opt)
	(:shadow (shadow (mapcar #'string (rest opt)) pack))
	(:shadowing-import-from
	 (let ((from-pack (find-package (second opt))))
	   (dolist (sname (rest (rest opt)))
	     (multiple-value-bind (sym found)
				  (find-symbol (string sname) from-pack)
	       (if found
		   (shadowing-import sym pack)
		   (error "no symbol named ~s in package ~s"
			  (string sname)
			  from-pack))))))))
    (dolist (opt options)
      (if (eq (first opt) :use)
	  (use-package (mapcar #'string (rest opt)) pack)))
    (dolist (opt options)
      (case (first opt)
	(:intern
	 (dolist (sname (rest opt)) (intern (string sname) pack)))
	(:import-from
	 (let ((from-pack (find-package (second opt))))
	   (dolist (sname (rest (rest opt)))
	     (multiple-value-bind (sym found)
				  (find-symbol (string sname) from-pack)
	       (if found
		   (import sym pack)
		   (error "no symbol named ~s in package ~s"
			  (string sname)
			  from-pack))))))))
    (dolist (opt options)
      (if (eq (first opt) :export)
	  (dolist (sname (rest opt))
		  (export (intern (string sname) pack) pack))))
    pack))

(defun apropos (x &optional package)
  (if package
      (do-symbols (s package)
        (if (string-search x s) (format t "~s~%" s)))
      (do-all-symbols (s)
        (if (string-search x s) (format t "~s~%" s))))
  (values))

(defun apropos-list (x &optional package)
  (let ((res nil))
    (if package
	(do-symbols (s package res)
	  (if (string-search x s) (push s res)))
        (do-all-symbols (s res)
	  (if (string-search x s) (push s res))))))


;;;;;
;;;;; Additional Multiple Value Functions and Macros
;;;;;

(export
 '(values-list multiple-value-list multiple-value-bind multiple-value-setq))

(defun values-list (x) (apply #'values x))

(defmacro multiple-value-list (form)
  `(multiple-value-call #'list ,form))

(defmacro multiple-value-bind (vars form &rest body)
  (let ((rsym (gensym)))
    `(multiple-value-call #'(lambda (&optional ,@vars &rest ,rsym)
			      (declare (ignore ,rsym))
			      ,@body)
			,form)))

(defmacro multiple-value-setq (variables form)
  (let* ((tvars (mapcar #'(lambda (x) (gensym "V")) variables))
	 (pairs nil))
    (mapc #'(lambda (x y) (push y pairs) (push x pairs)) variables tvars)
    (if (null tvars) (push (gensym) tvars))
    `(multiple-value-bind ,tvars ,form (setq ,@pairs) ,(first tvars))))


;;;;
;;;; Fixup some functions
;;;;

(if (find-symbol "SAVE") (unexport 'save))
(if (find-symbol "RESTORE") (unexport 'restore))

(export '(save-workspace add-exit-function))

(defun save-workspace (name)
  (loop
   (unless (consp system::*exit-functions*) (return))
   (let ((func (pop system::*exit-functions*)))
     (ignore-errors (funcall func))))
  (dolist (h (copy-list *hardware-objects*))
    (send (third h) :remove))
  (save name)
  (exit))

(defun add-exit-function (fun)
  (push fun system::*exit-functions*))


;;;;
;;;; DECLARE and PROCLAIM
;;;;

(export '(declare proclaim special))

(defmacro declare (&rest args)
  (if *displace-macros*
      (dolist (a args)
        (if (eq (first a) 'special)
	    (return (warn "special declarations are not supported"))))))

(defun proclaim (decl)
  (if (eq (first decl) 'special)
      (dolist (s (rest decl))
        (mark-as-special s))))

;;;;
;;;; More printing stuff
;;;;

(export '(write write-string write-line finish-output clear-output
	  *print-pretty* *print-circle* *print-radix* *print-base*
	  *print-array* *read-base*
	  write-to-string prin1-to-string princ-to-string))

(defparameter *print-pretty* nil)
(defparameter *print-circle* nil)
(defparameter *print-radix* nil)
(defparameter *print-base* 10.)
(defparameter *print-array* t)

(defparameter *read-base* 10.)

(defun write (x &key
		((:escape *print-escape*) *print-escape*)
		((:gensym *print-gensym*) *print-gensym*)
		((:readably *print-readably*) *print-readably*)
		((:length *print-length*) *print-length*)
		((:level *print-level*) *print-level*)
		((:case *print-case*) *print-case*)
		((:circle *print-circle*) *print-circle*)
		((:radix *print-radix*) *print-radix*)
		((:base *print-base*) *print-base*)
		((:array *print-array*) *print-array*)
		((:pretty *print-pretty*) *print-pretty*)
		(stream *standard-output*))
  (format stream (if *print-escape* "~s" "~a") x)
  x)

(defun write-string (string &optional (stream *standard-output*)
			    &key (start 0) end)
  (format stream "~a" (subseq string start end))
  string)

(defun write-line (string &optional (stream *standard-output*)
			    &key (start 0) end)
  (format stream "~a~%" (subseq string start end))
  string)

(defun finish-output (&optional (s *standard-output*)) (force-output s))

(defun clear-output (&optional (s *standard-output*)) (force-output s))

(defun write-to-string (x &rest args)
  (with-output-to-string (s) (apply #'write x :stream s args)))

(defun prin1-to-string (arg) (format nil "~s" arg))
(defun princ-to-string (arg) (format nil "~a" arg))


;;;;
;;;; Miscellaneous
;;;;

(export '(keywordp schar nreconc))

(defsetf char (x i) (v) `(setf (aref ,x ,i) ,v))

(defun keywordp (x)
  (and (symbolp x) (eq (symbol-package x) (find-package "KEYWORD"))))

(setf (symbol-function 'schar) #'char)

(defun nreconc (x y) (nconc (nreverse x) y))


(export '(describe svref lambda-list-keywords get-properties))

(defun describe (x &optional (stream t))
  (format stream "~&~s - ~a~%" x (type-of x)))

(setf (symbol-function 'svref) #'aref)

(defconstant lambda-list-keywords
  '(&optional &rest &key &allow-other-keys &aux &whole &environment &body))

(defun get-properties (place ilist)
  (let ((dflt (cons nil nil)))
    (dolist (i ilist (values nil nil nil))
      (let ((v (getf place i)))
	(unless (eq v dflt) (return (values i v t)))))))

(export '(parse-integer compiled-function-p packagep gentemp bit-vector-p))

(defun parse-integer (s &key (start 0) end radix junk-allowed)
  (read-from-string (subseq s start end)))

;**** need to rule out macros
(defun compiled-function-p (f)
  (typep f '(or subr byte-code-closure)))

(defun packagep (x) (typep x 'package))

(defvar *gentemp-counter* -1)

(defun gentemp (&optional (prefix "T") (package *package*))
  (loop
   (incf *gentemp-counter*)
   (let ((name (format nil "~a~d" prefix *gentemp-counter*)))
     (unless (find-symbol name package) (return (intern name package))))))

(defun bit-vector-p (x) nil)

;;;;
;;;; DESTRUCTURING-BIND
;;;; Simple implementation that only allows destructuring required arguents.
;;;; Nothing is done with declarations at this point.
;;;;

(export 'destructuring-bind)

;;**** This can probably be made more efficient with little extra work.
;;**** If the compiler optimizes out the apply calls, this work may
;;**** not be worth it. Very complex code could produce deep recursions,
;;**** but most of the time there are only one or two destructuring arguments.
#|
(defun do-destructure (ll al body)
  (cond
   ((null ll) `(progn (destructure-check-arg ,al t) ,@body))
   ((and (eq (first ll) '&rest) (null (rest (rest ll))))
    `(let ((,(second ll) ,al)) ,@body))
   ((member (first ll) lambda-list-keywords)
    `(apply #'(lambda ,ll ,@body) ,al))
   ((consp (first ll))
    (let ((als (gensym)))
      `(let ((,als ,al))
	 (destructuring-bind ,(first ll) (first ,als)
			     ,(do-destructure (rest ll) `(rest ,als) body)))))
   ((and (symbolp (first ll)) (not (null (first ll))))
    (let ((als (gensym)))
      `(let* ((,als (destructure-check-arg ,al nil))
	      (,(first ll) (first ,als)))
	 ,(do-destructure (rest ll) `(rest ,als) body))))
   (t (error "bad formal argument list"))))
|#

(defun destructuring-arglist-p (x)
  (consp
   (find-if #'(lambda (x) (or (member x lambda-list-keywords) (consp x))) x)))

(defun do-destructure (ll al body)
  (let* ((args (destructure-fix-arglist ll))
	 (farg (first args)))
    (cond
     ((null args) `(progn (destructure-check-arg ,al t) ,@body))
     ((not (destructuring-arglist-p args))
      `(apply #'(lambda ,args ,@body) ,al))
     ((consp farg)
      (let ((asym (gensym)))
	`(let ((,asym ,al))
	   ,(do-destructure farg
			    `(first ,asym)
			    (list (do-destructure (rest args)
						  `(rest ,asym)
						  body))))))
     ((and (symbolp farg) (not (null farg)))
      (let ((asym (gensym)))
	`(let* ((,asym ,al)
		(,farg (progn (destructure-check-arg ,asym nil)
			      (first ,asym))))
	   ,(do-destructure (rest args) `(rest ,asym) body))))
     (t (error "bad formal argument list")))))

(defun destructure-check-arg (x toomany)
  (if toomany
      (unless (null x) (error "too many arguments"))
      (unless (consp x) (error "too few arguments")))
  x)

(defun destructure-fix-arglist (ll)
  (setf ll (copy-list ll))
  (let ((last (last ll)))
    (unless (null (cdr last))
	    (rplacd last (list '&rest (cdr last)))))
  (let ((p (position '&body ll)))
    (if p (setf (nth p ll) '&rest)))
  ll)

(defmacro destructuring-bind (args vals &rest body)
  (do-destructure (destructure-fix-arglist args) vals body))

(defmacro destructuring-bind (args vals &rest body)
  (do-destructure args vals body))


;;;;
;;;; SPECIAL-FORM-P
;;;;

(export '(special-form-p))

(defun special-form-p (x)
  (and (symbolp x) (fboundp x) (typep (symbol-function x) 'fsubr)))


;;;;
;;;; COMPLEMENT
;;;;

(export 'complement)

(defun complement (f) #'(lambda (&rest args) (not (apply f args))))


;;;
;;; COPY-STRUCTURE and COPY-SYMBOL
;;;

(export '(copy-structure copy-symbol))

(defun copy-structure (struct) (xlisp::%copy-struct struct))

(defun copy-symbol (sym &optional props)
  (let ((newsym (make-symbol (copy-seq (symbol-name sym)))))
    (when props
          (when (boundp sym)
                (setf (symbol-value newsym) (symbol-value sym)))
          (when (fboundp sym)
                (setf (symbol-function newsym) (symbol-function sym)))
          (setf (symbol-plist newsym) (copy-list (symbol-plist sym))))
    newsym))


;;;;
;;;; TAILP and LDIFF
;;;;
;;;; Definitions taken from HyperSpec.

(export '(tailp ldiff))

(defun tailp (object list)
  (do ((list list (cdr list)))
      ((atom list) (eql list object))
      (if (eql object list)
          (return t))))

(defun ldiff (list object)
  (do ((list list (cdr list))
       (r '() (cons (car list) r)))
      ((atom list)
       (if (eql list object) (nreverse r) (nreconc r list)))
      (when (eql object list)
	    (return (nreverse r)))))

;;;;
;;;; MEMBER-IF
;;;;

(export 'member-if)

(defun member-if (pred list &key key)
  (loop
   (when (null list) (return nil))
   (when (funcall pred (if key (funcall key (car list)) (car list)))
         (return list))
   (pop list)))

;;;;
;;;; ISQRT
;;;;

(export 'isqrt)

;;**** ckeck this; look into vectorization
(defun isqrt (x)
  (if (and (typep x 'integer) (not (minusp x)))
      (do* ((est (ash 1 (truncate (integer-length x) 2))
		 (truncate (+ est est2) 2))
	    (est2 (truncate x est) (truncate x est)))
	   ((> 2 (abs (- est est2))) (min est est2)))
    (floor (sqrt x))))
