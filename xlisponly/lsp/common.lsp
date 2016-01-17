;; common.lsp. functions missing that are part of common lisp,
;; and commonly used

;; It is assumed you are using XLISP-PLUS 3.0 with all Common Lisp related 
;; options (except packages) turned on before you load this file.

;; Author either unknown or Tom Almy unless indicated.

(in-package "XLISP")

; (unintern sym) - remove a symbol from the oblist
#-:packages
(defun unintern (symbol)
  (let ((subhash (hash symbol (length *obarray*))))
    (cond ((member symbol (aref *obarray* subhash))
             (setf (aref *obarray* subhash)
                   (delete symbol (aref *obarray* subhash)))
             t)
          (t nil))))

(export '(pairlis copy-list copy-alist copy-tree signum))

;; pairlis does not check for lengths of keys and values being unequal

(defun pairlis (keys values &optional list)
       (nconc (mapcar #'cons keys values) list))

(defun copy-list (list) (append list 'nil))

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
   (cond ((not (numberp x)) (error "~s is not a number" x))
         ((zerop (abs x)) x)
	 (t (/ x (abs x)))))

(export '(remf incf decf push pushnew pop))

; Cruddy but simple versions of these functions.
; Commented out since XLISP will now expand macros once, making
; good version much preferred.

;(defmacro incf (var &optional (delta 1))
;    `(setf ,var (+ ,var ,delta)))

;(defmacro decf (var &optional (delta 1))
;    `(setf ,var (- ,var ,delta)))

;(defmacro push (v l)
;	`(setf ,l (cons ,v ,l))))

;(defmacro pushnew (a l &rest args)
;  `(unless (member ,a ,l ,@args) (push ,a ,l) nil))

;(defmacro pop (l)
;	`(prog1 (first ,l) (setf ,l (rest ,l)))))


; This is what one really needs to do for incf decf and
; (in common.lsp) push and pop. The setf form must only be evaluated once.
; But is it worth all this overhead for correctness?
; (By Tom Almy)

(defun |DoForm| (form) ; returns (cons |list for let| |new form|)
       (let* ((args (rest form)) ; raw form arguments
	      (letlist (mapcan #'(lambda (x) (when (consp x)
						   (list (list (gensym) x))))
			       form))
	      (revlist (mapcar #'(lambda (x) (cons (second x) (first x)))
			       letlist))
	      (newform (cons (first form) (sublis revlist args))))
	     (cons letlist newform)))

(defun |RemProp| (l prop)
       (do ((cl l (cddr cl))
	    (flg nil cl))
	   ((atom cl) nil)	; none found
	   (cond ((atom (cdr l))
		  (error "odd length property list"))
		 ((eq (car cl) prop) ; a match!
		  (if flg ; different if first in list from later
		      (rplacd (cdr flg) (cddr cl))
		      (setq l (cddr l)))
		  (return (list l))))))

(defmacro remf (form prop &aux (remres (gensym)))
	  (if (and (consp form) (some #'consp form))
	      (let ((retval (|DoForm| form)))
		   `(let* ( ,@(car retval)
			    (,remres (|RemProp| ,(cdr retval) ,prop)))
			  (if ,remres
			      (progn (setf ,(cdr retval) (car ,remres))
				     t)
			      nil)))
	      `(let ((,remres (|RemProp| ,form ,prop)))
		    (if ,remres (progn (setf ,form (car ,remres)) t)
			nil))))

#-:packages
(unintern '|RemProp|)

(defmacro incf (form &optional (delta 1))
	  (if (and (consp form) (some #'consp form))
	      (let ((retval (|DoForm| form)))
		   `(let ,(car retval)
			 (setf ,(cdr retval)
			       (+ ,(cdr retval) ,delta))))
	      `(setf ,form (+ ,form ,delta))))

(defmacro decf (form &optional (delta 1))
	  (if (and (consp form) (some #'consp form))
	      (let ((retval (|DoForm| form)))
		   `(let ,(car retval)
			 (setf ,(cdr retval)
			       (- ,(cdr retval) ,delta))))
	      `(setf ,form (- ,form ,delta))))

(defmacro push (val form)
	  (if (and (consp form) (some #'consp form))
	      (let ((retval (|DoForm| form)))
		   `(let ,(car retval)
			 (setf ,(cdr retval)
			       (cons ,val ,(cdr retval)))))
	      `(setf ,form (cons ,val ,form))))

(defmacro pop (form)
	  (if (and (consp form) (some #'consp form))
	      (let ((retval (|DoForm| form)))
		   `(let ,(car retval)
			 (prog1 (first ,(cdr retval))
				(setf ,(cdr retval)
				      (rest ,(cdr retval))))))
	      `(prog1 (first ,form)
		      (setf ,form (rest ,form)))))


(defmacro pushnew (val form &rest rest)
	  (if (and (consp form) (some #'consp form))
	      (let ((retval (|DoForm| form)))
		   `(let ,(car retval)
			 (setf ,(cdr retval)
			       (adjoin ,val ,(cdr retval) ,@rest))))
	      `(setf ,form (adjoin ,val ,form ,@rest))))

; DoForm is now needed in COMMON2.LSP
; #-:packages
; (unintern '|DoForm|)

;; Hyperbolic functions    Ken Whedbee  from CLtL

(export '(logtest cis sinh cosh tanh asinh acosh atanh))

#-:bignums (defun logtest (x y) (not (zerop (logand x y))))

(defconstant imag-one #C(0.0 1.0))

(defun cis (x) (exp (* imag-one x)))


(defun sinh (x) (/ (- (exp x) (exp (- x))) 2.0))
(defun cosh (x) (/ (+ (exp x) (exp (- x))) 2.0))
(defun tanh (x) (/ (sinh x) (cosh x)))

(defun asinh (x) (log (+ x (sqrt (+ 1.0 (* x x))))))
(defun acosh (x)
       (log (+ x
               (* (1+ x)
                    (sqrt (/ (1- x) (1+ x)))))))
(defun atanh (x)
       (when (or (= x 1.0) (= x -1.0))
             (error "~s is a logarithmic singularity" x))
       (log (/ (1+ x) (sqrt (- 1.0 (* x x))))))



;; Additional Common Lisp Functions by Luke Tierney
;; from xlisp-stat

;;
;; Defsetf and documentation functions
;; Corrected for Common Lisp compatibility (requires XLISP-PLUS 2.1e or later)
;;  Modified by Tom Almy, 7/92
;;  Corrected again in 6/93
;;  and again (Luke Tierney) 11/93
;;

(export '(defsetf))

(defun apply-arg-rotate (f args)
  (apply f (list 'quote (car (last args))) (butlast args)))

; (defsetf) - define setf method
(defmacro defsetf (sym first &rest rest)
  (if (symbolp first)
      `(progn (setf (get ',sym '*setf*) #',first)
	      (remprop ',sym '*setf-lambda*)
	      ',sym)
      (let ((f `#'(lambda ,(append (car rest) first) ,@(cdr rest)))
            (args (gensym)))
        `(progn
          (setf (get ',sym '*setf-lambda*) ; changed *setf* to *setf-lambda*
                #'(lambda (&rest ,args) (apply-arg-rotate ,f ,args)))
	  (remprop ',sym '*setf*)
          ',sym))))


;;;;
;;;;
;;;; *Modules*, provide and require:  Leo Sarasua
;;;;
;;;;

(export '(provide require *modules*)) ; LSG

(defvar *modules*)

(defun provide (name)        ; LSG
       (pushnew (string name) *modules* :test #'string=))

(defun require (name &optional (pathname)) ; LSG
       (let ((name (string name))
	     (path (string pathname)))
	    (or (find name *modules* :test #'string=)
		(load (strcat pathname name)) )))

(defun require (name &optional (pathname)) ; LSG
       (let ((namelist (mapcar #'string (if (listp name) name (list name))))
	     (path (string pathname)))
	    (dolist (name1 namelist)
		    (or (find name1 *modules* :test #'string=)
			(load (strcat pathname name1)) ))))



;;;;
;;;;
;;;; Miscellaneous Functions:  Luke Tierney
;;;;    from xlisp-stat
;;;;

(export '(equalp y-or-n-p yes-or-no-p functionp with-input-from-string
          with-output-to-string with-open-file))

; equalp rewritten by Tom Almy to better match Common Lisp
(defun equalp (x y)
  (cond ((equal x y) t)
      ((numberp x) (if (numberp y) (= x y) nil))
      ((characterp x) (if (characterp y) (char-equal x y) nil))
      ((and (or (arrayp x) (stringp x))
            (or (arrayp y) (stringp y))
            (eql (length x) (length y)))
       (every #'equalp x y))))

; Modified by TAA
#-:getkey
(defun y-or-n-p (&rest args)
       (reset-system)
       (when args (fresh-line) (apply #'format *terminal-io* args))
       (do ((answer (string-trim " " (read-line))
		    (string-trim " " (read-line))))
	   ((or (string-equal answer "Y")
		(string-equal answer "N"))
	    (string-equal answer "Y"))
	   (princ " Answer \"y\" or \"n\": " *terminal-io*)))

#+:getkey
(defun y-or-n-p (&rest args)
       (when args (fresh-line) (apply #'format *terminal-io* args))
       (do ((answer (princ (int-char (get-key)))
		    (princ (int-char (get-key)))))
	   ((or (char-equal answer #\Y)
		(char-equal answer #\N))
	    (char-equal answer #\Y))
	   (princ "\nAnswer \"y\" or \"n\": " *terminal-io*)))


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

; Improved by TAA to match common lisp definition
(defun functionp (x)
    (if (typep x '(or closure subr symbol))
	t
        (and (consp x) (eq (car x) 'lambda))))

;(defmacro with-input-from-string (stream-string &rest body)
;  (let ((stream (first stream-string))
;        (string (second stream-string)))
;    `(let ((,stream (make-string-input-stream ,string)))
;       (progn ,@body))))


(defmacro with-input-from-string
	  (stream-string &rest body)
	  (let ((stream (first stream-string))
		(string (second stream-string))
		(start (second (member :start (cddr stream-string))))
		(end (second (member :end (cddr stream-string))))
		(index (second (member :index (cddr stream-string)))))
	       (when (null start) (setf start 0))
	       (if index
		   (let ((str (gensym)))
		    `(let* ((,str ,string)
			    (,stream (make-string-input-stream ,str
							       ,start
							       ,end)))
			   (prog1 (progn ,@body)
				  (setf ,index
					(- (length ,str)
					   (length (get-output-stream-list
						     ,stream)))))))
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

(export '(eval-when declare proclaim special))
;; Dummy function to allow importing CL code
(defmacro eval-when (when &rest body)
  (if (or (member 'eval when) (member 'execute when))
      `(progn ,@body)))

(defmacro declare (&rest args)
  (if *displace-macros*
      (dolist (a args)
        (if (eq (first a) 'special)
	    (return (cerror "special ignored"
			    "special declarations are not supported"))))))
(defun proclaim (decl)
  (if (eq (first decl) 'special)
      (dolist (s (rest decl))
        (mark-as-special s))))


;; array functions.   KCW    from  Kyoto Common Lisp

(export '(fill replace acons))

(defun fill (sequence item
             &key (start 0) end)
       (when (null end) (setf end (length sequence)))
       (do ((i start (1+ i)))
	   ((>= i end) sequence)
	   (setf (elt sequence i) item)))


(defun replace (sequence1 sequence2
                &key (start1 0) end1
                     (start2 0) end2)
    (when (null end1) (setf end1 (length sequence1)))
    (when (null end2) (setf end2 (length sequence2)))
    (if (and (eq sequence1 sequence2)
             (> start1 start2))
        (do* ((i 0 (1+ i))
              (l (if (< (- end1 start1) (- end2 start2))
                     (- end1 start1)
                     (- end2 start2)))
              (s1 (+ start1 (1- l)) (1- s1))
              (s2 (+ start2 (1- l)) (1- s2)))
            ((>= i l) sequence1)
          (setf (elt sequence1 s1) (elt sequence2 s2)))
        (do ((i 0 (1+ i))
             (l (if (< (- end1 start1)(- end2 start2))
                    (- end1 start1)
                    (- end2 start2)))
             (s1 start1 (1+ s1))
             (s2 start2 (1+ s2)))
            ((>= i l) sequence1)
          (setf (elt sequence1 s1) (elt sequence2 s2)))))


(defun acons (x y a)         ; from CLtL
   (cons (cons x y) a))


;; more set functions.  KCW    from Kyoto Common Lisp

;; Modified to pass keys to subfunctions without checking here
;; (more efficient)

;; (Tom Almy states:) we can't get the destructive versions of union
;; intersection, and set-difference to run faster than the non-destructive
;; subrs. Therefore we will just have the destructive versions do their
;; non-destructive counterparts

(export '(nunion nintersection nset-difference
	  set-exclusive-or nset-exclusive-or))

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



;;;;;
;;;;; Symbol and Package Functions
;;;;;
#+:packages
(export '(defpackage do-symbols do-external-symbols do-all-symbols
	  apropos apropos-list))

#+:packages
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

#+:packages
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

#+:packages
(defmacro do-external-symbols (spr &rest body)
  (let ((packsym (gensym))
	(arraysym (gensym)))
    `(let* ((,packsym ,(if (second spr) (second spr) '*package*))
	    (,arraysym (list (package-obarray ,packsym))))
       (do-symbol-arrays ,(first spr) ,(third spr) ,arraysym ,body))))

#+:packages
(defmacro do-all-symbols (sr &rest body)
  (let ((packsym (gensym))
	(arraysym (gensym)))
    `(let* ((,packsym (list-all-packages))
	    (,arraysym nil))
       (dolist (p ,packsym)
         (push (package-obarray p) ,arraysym)
	 (push (package-obarray p nil) ,arraysym))
       (do-symbol-arrays ,(first sr) ,(second sr) ,arraysym ,body))))

#+:packages
(defmacro defpackage (pname &rest options)
  `(let* ((pname ',pname)
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
				 (append nicknames (package-nicknames pack))))
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
     pack))

#+:packages
(defun apropos2 (s)
       (format t "~&~s" s)
       (when (fboundp s) (format t "  Function"))
       (if (constantp s)
	   (format t "  Constant=~s" (symbol-value s))
	   (when (boundp s) (format t "  Value=~s" (symbol-value s)))))

#+:packages
(defun apropos (x &optional package)
       (if package
	   (do-symbols (s package)
		       (if (search x (string s) :test #'char-equal)
			   (apropos2 s)))
	   (do-all-symbols (s)
			   (if (search x (string s) :test #'char-equal)
			       (apropos2 s))))
       (values))

#+:packages
(defun apropos-list (x &optional package)
       (let ((res nil))
	    (if package
		(do-symbols (s package res)
			    (if (search x (string s) :test #'char-equal)
				(push s res)))
		(do-all-symbols (s res)
				(if (search x (string s) :test #'char-equal)
				    (push s res))))))


;;;;;
;;;;; Additional Multiple Value Functions and Macros
;;;;;

(export
 '(values-list multiple-value-list multiple-value-bind multiple-value-setq))

(defun values-list (x) (apply #'values x))

(defmacro multiple-value-list (form)
  `(multiple-value-call #'list ,form))

(defmacro multiple-value-bind (vars form &rest body)
  `(multiple-value-call #'(lambda (&optional ,@vars &rest ,(gensym)) ,@body)
			,form))

(defmacro multiple-value-setq (variables form)
  (let* ((tvars (mapcar #'(lambda (x) (gensym "V")) variables))
	 (pairs nil))
    (mapc #'(lambda (x y) (push y pairs) (push x pairs)) variables tvars)
    (if (null tvars) (push (gensym) tvars))
    `(multiple-value-bind ,tvars ,form (setq ,@pairs) ,(first tvars))))

(push :common *features*)
