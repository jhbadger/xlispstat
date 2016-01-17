; common2.lsp. Fill in "missing" Common Lisp definitions
; However, those that concern features of Common Lisp not implemented in
; XLISP (such as the compiler and arrays) are not implemented here.

; You might want to only include those functions your application needs.

; Tom Almy, 7/93, revised 4/97

#-:common (load "common")  ; We need these always!

(in-package "XLISP")

(export
   '(array-in-bounds-p assert assoc-if assoc-if-not
     ccase char-code-limit char-name character check-type
     copy-seq copy-symbol ctypecase delete-duplicates ecase
     etypecase fifth sixth seventh eighth ninth tenth float-sign
     hash-table-p isqrt keywordp packagep ldiff make-list
     make-string make-sequence member-if member-if-not merge
     mismatch nbutlast nreconc prin1-to-string princ-to-string
     probe-file rassoc rassoc-if rassoc-if-not read-from-string realp
     revappend tailp typecase sleep with-open-stream byte byte-size
     byte-position ldb ldb-test mask-field dpb deposit-field the ))

(defconstant char-code-limit 128)

;; Byte functions Added by Tom Almy, 4/95

;; "alias" these to common list functions
(setf (symbol-function 'byte) (symbol-function 'cons))
(setf (symbol-function 'byte-size) (symbol-function 'car))
(setf (symbol-function 'byte-position) (symbol-function 'cdr))

(defmacro bytetest (x)
       `(unless (and
		 (consp ,x)
		 (typep (byte-size ,x) 'fixnum)
		 (typep (byte-position ,x) 'fixnum)
		 (>= (byte-size ,x) 0)
		 (>= (byte-position ,x) 0))
		(error "arguments must be non-negative fixnums")))

(defun ldb (byte x)
       (bytetest byte)
       (logand (ash x (- (byte-position byte)))
	       (lognot (ash -1 (byte-size byte)))))
(defun ldb-test (byte x)
       (bytetest byte)
       (not (zerop (ldb byte x))))
(defun mask-field (byte x)
       (bytetest byte)
       (logand (ash (lognot (ash -1 (byte-size byte))) (byte-position byte))
	       x))
(defun dpb (new byte x)
       (bytetest byte)
       (let ((mask (lognot (ash -1 (byte-size byte)))))
	    (logior (logand x
			    (lognot (ash mask (byte-position byte))))
		    (ash (logand new mask) (byte-position byte)))))
(defun deposit-field (new byte x)
       (bytetest byte)
       (logxor x
	       (logand (ash (lognot (ash -1 (byte-size byte)))
			    (byte-position byte))
		       (logxor x new))))


(defsetf ldb (byte x) (new)
	 (let ((nbyte (gensym)) (nnew (gensym)))
	      (if (and (consp x) (some #'consp x))
		  (let ((retval (|DoForm| x)))
		       `(let ((,nbyte ,byte) ,@(car retval) (,nnew ,new))
			     (setf ,(cdr retval)
				   (dpb ,nnew ,nbyte ,(cdr retval)))
			     ,nnew))
		  `(let ((,nbyte ,byte)(,nnew ,new))
			(setf ,x (dpb ,nnew ,nbyte ,x)) ,nnew))))



(defsetf mask-field (byte x) (new)
	 (let ((nbyte (gensym)) (nnew (gensym)))
	      (if (and (consp x) (some #'consp x))
		  (let ((retval (|DoForm| x)))
		       `(let ((,nbyte ,byte) ,@(car retval) (,nnew ,new))
			     (setf ,(cdr retval)
				   (deposit-field ,nnew ,nbyte ,(cdr retval)))
			     ,nnew))
		  `(let ((,nbyte ,byte)(,nnew ,new))
			(setf ,x (deposit-field ,nnew ,nbyte ,x)) ,nnew))))

; Simplistic versions, probably goodenuf, the problem is that they
; do multiple evaluations
; (defsetf ldb (byte x) (new)
;	 `(progn (setf ,x (dpb ,new ,byte ,x)) ,new))
;(defsetf mask-field (byte x) (new)
;	 `(progn (setf ,x (deposit-field ,new ,byte ,x)) , new))

;; Apropos function contributed by Jan Kok, kok@cs.ColoState.edu

#-:packages
(defun apropos-list (string)
       (let ((str (string string)) ; convert to string, if symbol is passed
	     (result nil))
	    (dotimes (i (length *obarray*) result)
		     (dolist (sym (elt *obarray* i))
			     (if (search str (string sym) :test #'char-equal)
				 (push sym result))))))
#-:packages
(defun apropos (string)
       (mapcar #'(lambda (x)
			 (format t "~&~s" x)
			 (when (fboundp x) (princ " (defined)"))
			 (when (boundp x) (format t " value: ~s" x)))
	       (apropos-list string))
#+:mulvals   (values)
#-:mulvals   nil ; CL says return "nothing" but we need to return NIL
)

(defun array-in-bounds-p (a n) (and (>= n 0 (< n (length a)))))

(defmacro assert (test &optional forms &rest x &aux (loop (gensym)))
	  `(prog ((*print-length* 4) (*print-level* 4))
		 ,loop
		 (when ,test (return nil))
		 (cerror "requests new values for ~*~s"
			 "~a"			
			 ,(if (null x) "" `(format nil ,@x))
			 ',forms)
		 ,@(mapcan #'(lambda (i)
				     `((format t "Enter new value for ~s:" ',i)
				       (setf ,i (read))))
			   forms)
		 (go ,loop)))

(defun assoc-if (test form &key (key #'identity))
       (do ()
	   ((endp form) nil)
	   (when (and (consp (car form))
		      (funcall test (funcall key (caar form))))
		 (return (car form)))
	   (setq form (cdr form))))

(defun assoc-if-not (test form &key (key #'identity))
       (do ()
	   ((endp form) nil)
	   (when (and (consp (car form))
		      (not (funcall test (funcall key (caar form)))))
		 (return (car form)))
	   (setq form (cdr form))))

(defmacro ccase (keyform &rest x)
	  (let ((val (gensym))
		(loop (gensym))
		(values (mapcan #'(lambda (x)
					  (if (listp (car x))
					      (copy-list (car x))
					      (list (car x))))
				x)))
	       `(prog (,val) ,loop
		      (setf ,val ,keyform)
		      (return
		       (case ,val
			     ,@x
			     (t (cerror
				 "requests new value for ~s"
				 "~*~s fell through ccase expression, wanted one of ~s"
				 ',keyform
				 ,val
				 ',values)
				(format t "New value:")
				(setf ,keyform (read))
				(go ,loop)))))))

(defun char-name (c)
       (case c
	     (#\Space   "Space")
	     (#\Newline "Newline")
	     (#\Rubout  "Rubout")))

(defun character (arg) (coerce arg 'character))

(defmacro check-type (place type
			    &optional (string `',type)
			    &aux (loop (gensym)))
       `(prog () ,loop
	      (when (typep ,place ',type) (return))
	      (cerror "requests new value for ~s"
		      "the value of ~s is ~s which is not of type ~a"
		      ',place
		      ,place
		      ,string)
	      (format t "New value:")
	      (setf ,place (read))
	      (go ,loop)))


(defun copy-seq (seq) (subseq seq 0))

(defun copy-symbol (sym &optional all)
       (unless (symbolp sym) (error "~s is not a symbol" sym))
       (let ((x (make-symbol (symbol-name sym))))
	    (when all
		  (when (boundp sym) (setf (symbol-value x) sym))
		  (when (fboundp sym)
			(setf (symbol-function x) (symbol-function sym)))
		  (setf (symbol-plist x) (symbol-plist sym)))
	    x))

(defmacro ctypecase (keyform &rest x)
	  (let ((val (gensym))
		(loop (gensym))
		(values (mapcan #'(lambda (x)
					  (if (listp (car x))
					      (list (copy-list (car x)))
					      (list (car x))))
				x)))
	       `(prog (,val) ,loop
		      (setq ,val ,keyform)
		      (return
		       (typecase ,val
			     ,@x
			     (otherwise (cerror
				 "requests new value for ~s"
				 "~*~s fell through ctypecase expression, wanted one of ~s"
				 ',keyform
				 ,val
				 ',values)
				(format t "New value:")
				(setf ,keyform (read))
				(go ,loop)))))))
						
(setf (symbol-function 'delete-duplicates)      ; cheat on this one
      (symbol-function 'remove-duplicates))

#-:packages
(defmacro do-all-symbols (arglist &rest body)
	  (when (not (consp arglist)) (error "Bad first argument"))
	  `(dolist ( ,(first arglist)
		     ',(mapcan #'copy-list (coerce *obarray* 'list))
		     ,@(rest arglist))
		   ,@body))

(defmacro ecase (keyform &rest x)
	  (let ((tmp (gensym))
		(values (mapcan #'(lambda (x)
					  (if (listp (car x))
					      (copy-list (car x))
					      (list (car x))))
				x)))
	       `(let ((,tmp ,keyform))
		     (case ,tmp ,@x
			   (t (error "case ~s not one of ~s" ,tmp ',values))))))

(defmacro etypecase (keyform &rest x)
	  (let ((tmp (gensym))
		(values (mapcan #'(lambda (x)
					  (if (listp (car x))
					      (list (copy-list (car x)))
					      (list (car x))))
				x)))
	       `(let ((,tmp ,keyform))
		     (typecase ,tmp ,@x
			   (otherwise (error "type of ~s not one of ~s" ,tmp ',values))))))

;; These are terribly inefficient (see the awful defsetf code in common.lsp)
(defsetf first (x) (y) `(progn (rplaca ,x ,y) ,y))  ; same as "car"
(defsetf rest (x) (y) `(progn (rplacd ,x ,y) ,y))   ; same as "cdr"

(defsetf second (x) (y) `(progn (rplaca (cdr ,x) ,y) ,y))
(defsetf third (x) (y) `(progn (rplaca (cddr ,x) ,y) ,y))
(defsetf fourth (x) (y) `(progn (rplaca (cdddr ,x) ,y) ,y))
(defsetf fifth (x) (y) `(progn (rplaca (cddddr ,x) ,y) ,y))
(defsetf sixth (x) (y) `(progn (rplaca (nthcdr 5 ,x) ,y) ,y))
(defsetf seventh (x) (y) `(progn (rplaca (nthcdr 6 ,x) ,y) ,y))
(defsetf eighth (x) (y) `(progn (rplaca (nthcdr 7 ,x) ,y) ,y))
(defsetf ninth (x) (y) `(progn (rplaca (nthcdr 8 ,x) ,y) ,y))
(defsetf tenth (x) (y) `(progn (rplaca (nthcdr 9 ,x) ,y) ,y))

(defsetf cadr (x) (y) `(progn (rplaca (cdr ,x) ,y) ,y))
(defsetf caar (x) (y) `(progn (rplaca (car ,x) ,y) ,y))
(defsetf cdar (x) (y) `(progn (rplacd (car ,x) ,y) ,y))
(defsetf cddr (x) (y) `(progn (rplacd (cdr ,x) ,y) ,y))
(defsetf caaar (x) (y) `(progn (rplaca (caar ,x) ,y) ,y))
(defsetf caadr (x) (y) `(progn (rplaca (cadr ,x) ,y) ,y))
(defsetf cadar (x) (y) `(progn (rplaca (cdar ,x) ,y) ,y))
(defsetf caddr (x) (y) `(progn (rplaca (cddr ,x) ,y) ,y))
(defsetf cdaar (x) (y) `(progn (rplacd (caar ,x) ,y) ,y))
(defsetf cdadr (x) (y) `(progn (rplacd (cadr ,x) ,y) ,y))
(defsetf cddar (x) (y) `(progn (rplacd (cdar ,x) ,y) ,y))
(defsetf cdddr (x) (y) `(progn (rplacd (cddr ,x) ,y) ,y))
(defsetf caaaar (x) (y) `(progn (rplaca (caaar ,x) ,y) ,y))
(defsetf caaadr (x) (y) `(progn (rplaca (caadr ,x) ,y) ,y))
(defsetf caadar (x) (y) `(progn (rplaca (cadar ,x) ,y) ,y))
(defsetf caaddr (x) (y) `(progn (rplaca (caddr ,x) ,y) ,y))
(defsetf cadaar (x) (y) `(progn (rplaca (cdaar ,x) ,y) ,y))
(defsetf cadadr (x) (y) `(progn (rplaca (cdadr ,x) ,y) ,y))
(defsetf caddar (x) (y) `(progn (rplaca (cddar ,x) ,y) ,y))
(defsetf cadddr (x) (y) `(progn (rplaca (cdddr ,x) ,y) ,y))
(defsetf cdaaar (x) (y) `(progn (rplacd (caaar ,x) ,y) ,y))
(defsetf cdaadr (x) (y) `(progn (rplacd (caadr ,x) ,y) ,y))
(defsetf cdadar (x) (y) `(progn (rplacd (cadar ,x) ,y) ,y))
(defsetf cdaddr (x) (y) `(progn (rplacd (caddr ,x) ,y) ,y))
(defsetf cddaar (x) (y) `(progn (rplacd (cdaar ,x) ,y) ,y))
(defsetf cddadr (x) (y) `(progn (rplacd (cdadr ,x) ,y) ,y))
(defsetf cdddar (x) (y) `(progn (rplacd (cddar ,x) ,y) ,y))
(defsetf cddddr (x) (y) `(progn (rplacd (cdddr ,x) ,y) ,y))

(defun fifth (x) (nth 4 x))
(defun sixth (x) (nth 5 x))
(defun seventh (x) (nth 6 x))
(defun eighth (x) (nth 7 x))
(defun ninth (x) (nth 8 x))
(defun tenth (x) (nth 9 x))

(defun float-sign (x &optional (y 1.0))
       (if (minusp x) (- (abs y))
	   (abs y)))

(defun hash-table-p (x) (typep x 'hash-table))

#-:bignums (defun isqrt (x) (floor (sqrt x)))

#+:bignums
(defun isqrt (x)
       (if (and (typep x 'integer) (not (minusp x)))
	   (do* ((est (ash 1 (truncate (integer-length x) 2))
		      (truncate (+ est est2) 2))
		 (est2 (truncate x est) (truncate x est)))
		((> 2 (abs (- est est2))) (min est est2)))
	   (floor (sqrt x))))


#+:packages
(defun keywordp (x) (eq (symbol-package x) (find-package "KEYWORD")))
#-:packages
(defun keywordp (x)
       (and (symbolp x) (eq #\: (aref (symbol-name x) 0))))

#+:packages
(defun packagep (x) (typep x 'package))

(defun ldiff (l m)
       (do ((list l (cdr list)))
	   ((endp list) (copy-list l))
	   (when (eq m list) (return (butlast l (length list))))))

; This won't make really big lists in MS-DOS environments
(defun make-list (n &key initial-element &aux val)
       (coerce (make-array n :initial-element initial-element) 'list))

; This will make bigger lists, but is about 5x slower
;(defun make-list (n &key initial-element &aux val)
;       (dotimes (i n val) (push initial-element val)))

; These won't make really big strings/sequences in MS-DOS environments, but
;  I'm not giving an alternative!
(defun make-string (n &key (initial-element #\space))
       (coerce (make-array n :initial-element initial-element) 'string))

(defun make-sequence (type n
			   &key (initial-element
				 (if (eq type 'string) #\space nil)))
       (coerce (make-array n :initial-element initial-element) type))

(defun member-if (test form &key (key #'identity))
       (do ()
	   ((endp form) nil)
	   (when (funcall test (funcall key (car form))) (return form))
	   (setq form (cdr form))))

(defun member-if-not (test form &key (key #'identity))
       (do ()
	   ((endp form) nil)
	   (unless (funcall test (funcall key (car form))) (return form))
	   (setq form (cdr form))))

(defun merge (type s1 s2 pred &key (key #'identity))
       (let* ((i1 0)
	      (i2 0)
	      (i3 0)
	      (l1 (length s1))
	      (l2 (length s2))
	      (l3 (+ l1 l2))
	      (res (make-sequence type l3)))
	     (loop
	      (when (eql i3 l3) (return res))
	      (setf (elt res i3)
		    (cond
		     ((eql i1 l1)
		      (prog1 (elt s2 i2) (incf i2)))
		     ((eql i2 l2)
		      (prog1 (elt s1 i1) (incf i1)))
		     ((funcall pred
			       (funcall key (elt s1 i1))
			       (funcall key (elt s2 i2)))
		      (prog1 (elt s1 i1) (incf i1)))
		     (t
		      (prog1 (elt s2 i2) (incf i2)))))
	      (incf i3))))

(defun mismatch (sequence1 sequence2
		    &key
		    (key #'identity)
		    test test-not
		    (start1 0) end1
		    (start2 0) end2)
       (when (null end1) (setq end1 (length sequence1)))
       (when (null end2) (setq end2 (length sequence2)))
       (when (and test test-not)
	     (error "cannot specify both :test and :test-not"))
       (if test-not
	   (setq test test-not test-not #'not)
	   (progn (setq test-not #'identity)
		  (unless test (setq test #'eql))))
       (do* ((s1 start1 (1+ s1))
	     (s2 start2 (1+ s2)))
	    ((or (>= s1 end1) (>= s2 end2))
	     (if (and (>= s1 end1) (>= s2 end2))
		 nil
		 s1))
	    (unless (funcall test-not
			     (funcall test
				      (funcall key (elt sequence1 s1))
				      (funcall key (elt sequence2 s2))))
		    (return s1))))

; There are numerous n* functions that I'm not implementing because
; They would be slower then the builtin, non-destructive equivalents.


(defun nbutlast (list &optional (n 1) &aux (l (length list)))
       (if (< n l)
	   (prog2 (setf (cdr (nthcdr (1- (- l n)) list)) nil) list)
	   nil))

(defun nreconc (l1 l2) (nconc (nreverse l1) l2))

(defun prin1-to-string (arg) (format nil "~s" arg))
(defun princ-to-string (arg) (format nil "~a" arg))

(defun probe-file (arg) (open arg :direction :probe))

(defun rassoc (i form &key (key #'identity) test test-not)
       (when (and test test-not)
	     (error "cannot specify both :test and :test-not"))
       (if test-not
	   (do ()
	       ((endp form) nil)
	       (when (and (consp (car form))
			  (not (funcall test-not i (funcall key (cdar form)))))
		     (return (car form)))
	       (setq form (cdr form)))
	   (progn (unless test (setq test #'eql))
		  (do ()
		      ((endp form) nil)
		      (when (and (consp (car form))
				 (funcall test i (funcall key (cdar form))))
			    (return (car form)))
		      (setq form (cdr form))))))

(defun rassoc-if (test form &key (key #'identity))
       (do ()
	   ((endp form) nil)
	   (when (and (consp (car form))
		      (funcall test (funcall key (cdar form))))
		 (return (car form)))
	   (setq form (cdr form))))

(defun rassoc-if-not (test form &key (key #'identity))
       (do ()
	   ((endp form) nil)
	   (when (and (consp (car form))
		      (not (funcall test (funcall key (cdar form)))))
		 (return (car form)))
	   (setq form (cdr form))))

#+:mulvals
(defun read-from-string (arg &optional (e1 t) e2 &key (start 0) end)
       (let* ((r nil)
	      (s (with-input-from-string (f arg :start start :end end :index r)
					 (read f e1 e2))))
	     (values s r)))

#-:mulvals  ; Doesn't return second value (index into arg where parse stopped)
(defun read-from-string (arg &optional (e1 t) e2 &key (start 0) end)
       (with-input-from-string (f arg :start start :end end)
			       (read f e1 e2)))


#+:bignums (defun realp (arg) (or (rationalp arg) (floatp arg)))
#-:bignums (defun realp (arg) (or (integerp arg) (floatp arg)))

(defun revappend (x y) (nconc (reverse x) y))

(defun tailp (s l)
       (do ()
	   ((endp l) nil)
	   (when (eq s l) (return t))
	   (setq l (cdr l))))

(defmacro typecase (keyform &rest x &aux (tmp (gensym)))
	  `(let ((,tmp ,keyform))
		(cond
		      ,@(mapcar #'(lambda (x)
					 (if (eq (first x) 'otherwise)
					     `(t ,@(rest x))
					     `((typep ,tmp ',(first x))
					       ,@(rest x))))
				x))))

(defun sleep (time)
       (let ((endtime (+ time (/ (get-internal-real-time)
				 internal-time-units-per-second))))
	    (loop (when (> (/ (get-internal-real-time)
			      internal-time-units-per-second)
			   endtime)
			(return nil)))))

(defmacro with-open-stream (stream-args &rest body)
	  `(let ((,(first stream-args) ,(second stream-args)))
		(unwind-protect
		 (progn ,@body)
		 (when ,(first stream-args) (close ,(first stream-args))))))


; the function, contributed by Leo Sarasua, lsarasua@epo.e-mail.com

(defmacro the (type form)
  (let ((eval-form (gensym "form")))
    `(let ((,eval-form ,form))
       (if (eql ',type (type-of ,eval-form))
           ,eval-form
           (error "The value of ~s is not of type ~s" ',form ',type) ))))

