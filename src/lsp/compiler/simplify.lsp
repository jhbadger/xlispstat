(in-package "XLSCMP")

;;***** rethink substitution of multiple value continuations
;;***** review for multiple value continuations
;;***** add simplifiers for values, values-list and other mulval stuff
;;***** add simplifiers for multiple-value-prog1 stuff

;;;;;
;;;;; Simplification Phase
;;;;;

;;;
;;; Tree simplification rule data base
;;; (assoc list lets rules be redefined during development)
;;;

(defvar *call-rules* nil)
(defvar *lambda-call-rules* nil)
(defvar *simplify-debug* nil)

(defun add-call-rule (name rule)
  (let ((c (assoc name *call-rules*)))
    (if c
	(setf (cdr c) rule)
        (push (cons name rule) *call-rules*))))

(defun add-lambda-call-rule (name rule)
  (let ((c (assoc name *lambda-call-rules*)))
    (if c
	(setf (cdr c) rule)
        (push (cons name rule) *lambda-call-rules*))))

(defun add-symbol-call-rule (sym name rule)
  (let ((c (assoc name (get sym 'call-simplification-rules))))
    (if c
	(setf (cdr c) rule)
        (push (cons name rule) (get sym 'call-simplification-rules)))))

(defmacro define-call-rule (name &rest args)
  `(add-call-rule ',name #'(lambda ,@args)))

(defmacro define-lambda-call-rule (name &rest args)
  `(add-lambda-call-rule ',name #'(lambda ,@args)))

(defmacro define-symbol-call-rule (sym name &rest args)
  `(add-symbol-call-rule ',sym ',name #'(lambda ,@args)))

(defun apply-simplification-rules (n)
  (let ((changed nil))
    (when (call-node-p n)
	  (dolist (r *call-rules*)
	    (let ((old-n (if *simplify-debug* (copy-node-tree n))))
	      (when (funcall (cdr r) n)
		    (setf changed t)
		    (when *simplify-debug* (pp-cps old-n) (pp-cps n))
		    (if *breakenable* (format t "call rule ~a~%" (car r))))))
	  (let ((f (call-node-function n)))
	    (cond
	     ((lambda-node-p f)
	      (dolist (r *lambda-call-rules*)
		(unless (lambda-node-p (call-node-function n)) (return))
		(let ((old-n (if *simplify-debug* (copy-node-tree n))))
		  (when (funcall (cdr r) n)
			(setf changed t)
			(when *simplify-debug* (pp-cps old-n) (pp-cps n))
			(if *breakenable*
			    (format t "lambda rule ~a~%" (car r)))))))
	     (t
	      ;;**** make sure it is a global function def!!
	      (let ((sym (leaf-node-value f)))
		(dolist (r (get sym 'call-simplification-rules))
		  (let ((nf (call-node-function n)))
		    (unless (gfun-eq nf sym) (return))
		    (let ((old-n (if *simplify-debug* (copy-node-tree n))))
		      (when (funcall (cdr r) n)
			    (setf changed t)
			    (when *simplify-debug* (pp-cps old-n) (pp-cps n))
			    (if *breakenable*
				(format t "symbol rule ~a ~a~%"
					sym (car r))))))))))))
    changed))


;;;
;;; Tree simplification control
;;;

(defun simplify-node (n)
  (do () ((not (apply-simplification-rules n))))
  (if (every #'node-simplified-p (node-children n))
      (set-node-simplified n t)))

(defun next-unsimplified-node (n)
  (if n
      (labels ((next-lower-or-equal (n)
		 (if (or (null n) (node-simplified-p n))
		     nil
		     (dolist (c (node-children n) n)
		       (let ((lc (next-lower-or-equal c)))
			 (if lc (return lc)))))))
        (let ((nl (next-lower-or-equal n)))
	  (if nl nl (next-unsimplified-node (node-parent n)))))))

(defun first-unsimplified-child (n)
  (dolist (c (node-children n))
    (unless (node-simplified-p c) (return c))))

(defun next-lower-or-equal (n)
  (loop
   (if (or (null n) (node-simplified-p n)) (return nil))
   (let ((nc (first-unsimplified-child n)))
     (if nc (setf n nc) (return n)))))

(defun next-unsimplified-node (n)
  (loop
   (if (null n) (return nil))
   (let ((nl (next-lower-or-equal n)))
     (if nl (return nl) (setf n (next-unsimplified-node (node-parent n)))))))

(defun simplify-tree (tree)
  (do ((n (next-unsimplified-node tree) (next-unsimplified-node n)))
      ((null n) tree)
      (simplify-node n)))

;;;
;;; Specific simplification rules
;;;

(defun var-eq (x y) (eq x y))

#|
(defun find-references (var tree)
  (let ((refs nil))
    (labels ((fr (n)
	       (do* ((ch (node-children n) (rest ch))
		     (c (first ch) (first ch))
		     (i 0 (+ i 1)))
		    ((null ch))
		    (if (leaf-node-p c)
			(if (var-eq var c) (push (cons n i) refs))
		        (fr c)))))
      (fr tree))
    refs))

(defmacro do-references (alist &rest body)
  (let ((form (first alist))
	(pos (second alist))
	(var (third alist))
	(tree (fourth alist))
	(res (nth 4 alist))
	(vsym (gensym "VAR"))
	(tsym (gensym "TREE"))
	(frsym (gensym "FR"))
	(nsym (gensym "N"))
	(chsym (gensym "CH"))
	(csym (gensym "C"))
	(isym (gensym "I"))
	(start (gensym "START"))
	(end (gensym "END")))
    `(let ((,vsym ,var)
	   (,tsym ,tree))
       (block nil
         (labels ((,frsym (,nsym)
		    (let* ((,chsym (node-children ,nsym))
			   (,csym (first ,chsym))
			   (,isym 0))
		      (tagbody
		       ,start
		       (if (null ,chsym) (go ,end))
		       (if (leaf-node-p ,csym)
			   (if (var-eq ,vsym ,csym)
			       (let ((,form ,nsym)
				     (,pos ,isym))
				 ,@body))
			   (,frsym ,csym))
		       (setq ,chsym (rest ,chsym)
			     ,csym (first ,chsym)
			     ,isym (+ ,isym 1))
		       (go ,start)
		       ,end))))
	     (,frsym ,tsym)
	     (return ,res))))))

(defun any-references-p (v n)
  (if (leaf-node-p n)
      (eq v n)
      (dolist (c (node-children n)) (if (any-references-p v c) (return t)))))

(defun any-references-p (v n) (do-references (f i v n) (return t)))
|#

(defun simple-value-p (v)
  (or (not (lambda-node-p v)) (simple-lambda-node-p v)))

(defun call-reference-p (r) (= (cdr r) 0))

(defun any-call-references-p (refs)
  (dolist (r refs nil) (if (call-reference-p r) (return t))))

;;****** fix for lambda list ++++++
;;**** fix for multiple-value-continuation /////
(defun substitute-value (val ref copy)
  (let ((form (car ref))
	(pos (cdr ref)))
    (unless (or (simple-value-p val) (not (call-reference-p ref)))
	    (error "can't substitute a non-simple function in call position"))
    (insert-node-tree (if copy (copy-node-tree val) val) form pos)
    (if (and (lambda-node-p val) (call-reference-p ref))
	(fixup-lambda-call-node form))
    (do ((p form (node-parent p)))
	((not (and p (node-simplified-p p))))
	(set-node-simplified p nil))))

;;**** only do lambdas in call position, maybe contin.?
;;
;; ((lambda (... x ...) ... x ...) ... y ...) => ((lambda (...) ... y ...) ...)
;; if x appears zero or one times, or if y is a value node, as long as
;; x is not set.
;;
(define-lambda-call-rule substitute-variables (n)
  (lambda-call-substitute-variables n))

#|
;;****** fix for lambda list ++++++
(defun lambda-call-substitute-variables (n &optional all)
  (let* ((f (call-node-function n))
	 (args (copy-list (lambda-node-arglist f)))
	 (changed nil))
    (dolist (var args)
      (let* ((refs (find-references var (lambda-node-body f)))
	     (val-index (position var (lambda-node-arglist f)))
	     (val (call-node-arg n val-index)))
	(cond
	 ((null refs)
	  (lambda-node-delete-arg f var)
	  (call-node-delete-arg-index n val-index)
	  (setf changed t))
	 ((= (length refs) 1)
	  (when (or (simple-value-p val) (not (any-call-references-p refs)))
		(lambda-node-delete-arg f var)
		(call-node-delete-arg-index n val-index)
		(substitute-value val (first refs) nil)
		(setf changed t)))
	 ((or all (leaf-node-p val))
	  (when (or (simple-value-p val) (not (any-call-references-p refs)))
		(lambda-node-delete-arg f var)
		(call-node-delete-arg-index n val-index))
	  (dolist (r refs)
	    (if (or (simple-value-p val) (call-reference-p r))
		(substitute-value val r t)))
	  (setf changed t)))))
    (when (null (lambda-node-arglist f))
	  (move-node-tree (lambda-node-body f) n)
	  (setf changed t))
    changed))
|#
;;**** make sure these are enough for initial substitution in convert to work
(defun any-set-references-p (refs)
  (dolist (r refs)
    (if (and (gfun-eq (call-node-function (car r)) '%setq)
	     (= (cdr r) 2))
	(return t))))

(defun safe-set-substitute-value-p (v)
  (or (constant-node-p v) (lambda-node-p v) (gfun-node-p v) (gvar-node-p v)))

;;**** fix for multiple-value-continuation /////
(defun lambda-call-substitute-variables (n &optional all not-set)
  (let* ((f (call-node-function n))
	 (args (copy-list (lambda-node-arglist f)))
	 (changed nil))
    (dolist (var args)
      (let* ((refs (find-references var (lambda-node-body f)))
	     (val-index (position var (lambda-node-arglist f)))
	     (val (call-node-arg n val-index)))
	(unless (or ;;(and not-set (any-set-references-p refs))
		    (and not-set (or (not (safe-set-substitute-value-p val))
				     (any-set-references-p refs)))
		    (multiple-value-continuation-node-p val));;**** change /////
		(cond
		 ((null refs)
		  (lambda-node-delete-arg f var)
		  (call-node-delete-arg-index n val-index)
		  (setf changed t))
		 ((= (length refs) 1)
		  (when (or (simple-value-p val)
			    (not (any-call-references-p refs)))
			(lambda-node-delete-arg f var)
			(call-node-delete-arg-index n val-index)
			(substitute-value val (first refs) nil)
			(setf changed t)))
		 ((or (leaf-node-p val)
		      (and all
			   (every #'leaf-node-p
				  (node-children (lambda-node-body val)))))
		  (when (or (leaf-node-p val) (every #'call-reference-p refs))
			(lambda-node-delete-arg f var)
			(call-node-delete-arg-index n val-index))
		  (dolist (r refs)
			  (if (or (leaf-node-p val) (call-reference-p r))
			      (substitute-value val r t)))
		  (setf changed t))))))
    (when (null (lambda-node-arglist f))
	  (move-node-tree (lambda-node-body f) n)
	  (setf changed t))
    changed))


;;
;; ((lambda () <body>)) => <body>
;;
(define-lambda-call-rule no-variables (n)
  (let* ((f (call-node-function n))
	 (args (lambda-node-arglist f)))
    (if (null args)
	(progn (move-node-tree (lambda-node-body f) n)
	       t))))
			
(defun test-call-p (n) (gfun-eq (call-node-function n) '%test))


;;
;; (if (if a b c) d e) => (let ((x (lambda () d))
;;                              (y (lambda () e)))
;;                          (if a (if b (x) (y)) (if c (x) (y))))
;;
;;***** this is probably still wrong. It is important, but neds to be
;;***** thought through very carefully.
(defun lambda-node-real-body (n)
  (do* ((b (lambda-node-body n) (lambda-node-body f))
	(f (call-node-function b) (call-node-function b)))
       ((not (lambda-node-p f)) b)))

(defun shallow-copy-node-tree (n)
  (let ((ncopy (make-node)))
    (move-node-tree n ncopy)
    ncopy))

;;**** check this over carefully
(defun hoist-intervening-lambdas (n ba bf)
  (let* ((a (call-node-arg n 0))
	 (b (lambda-node-body a)))
    (if (lambda-node-p (call-node-function b))
	(let ((ncopy (shallow-copy-node-tree n)))
	  (move-node-tree b n)
	  (move-node-tree ba b)
	  (move-node-tree ncopy ba)))))	  

;;**** use shallow-copy here??
(defun hoist-test-continuations (n)
  (let* ((f (call-node-function n))
	 (nn (node-parent (node-parent (lambda-node-real-body f))))
	 (ba (lambda-node-body (call-node-arg nn 0)))
	 (xsym-node (make-leaf-node (gensym "X")))
	 (ysym-node (make-leaf-node (gensym "Y")))
	 (xcopy (copy-node-tree (call-node-arg ba 0)))
	 (ycopy (copy-node-tree (call-node-arg ba 1)))
	 (ncopy (make-node)))
    (insert-node-tree xsym-node ba 1)
    (insert-node-tree ysym-node ba 2)
    (move-node-tree nn ncopy)
    ;; This substitution forces insertion of the elligible
    ;; continuation.  Without this you get into a cycle. I have not
    ;; proved that with it you don't, but I think you don't --
    ;; something like the number of elligible nodes strictly
    ;; decreases.
    (lambda-call-substitute-variables ncopy t)
    (move-node-tree
     (make-call-node (make-lambda-node (list xsym-node ysym-node) ncopy)
		     xcopy
		     ycopy)
     nn)))

(define-lambda-call-rule if-if (n)
  (if (= (call-node-arg-count n) 1)
      (let ((f (call-node-function n))
	    (a (call-node-arg n 0)))
	(if (lambda-node-p a)
	    (let ((bf (lambda-node-body f))
		  (bba (lambda-node-body a))
		  (ba (lambda-node-real-body a)))
	      (if (and (test-call-p bf)
		       (test-call-p ba)
		       (every #'call-reference-p
			      (find-references (first (lambda-node-arglist f))
					       f)))
		  (let* ((bargs (lambda-node-arglist a))
			 (v (first bargs))
			 (vt (call-node-arg ba 2)))
		    (when (and (= (length bargs) 1)
			       (var-eq v vt)
			       (= (length (find-references v bba)) 1))
			  (hoist-intervening-lambdas n ba bf)
			  (hoist-test-continuations n)
			  t))))))))
		  
(defun leaf-node-constant-p (n)
  (if (leaf-node-p n)
      (let ((v (leaf-node-value n)))
	(or (null v) (eq v t) (not (symbolp v)))))) ;**** do this better

(defun constant-fold-numbers (n &optional f)
  (let ((args (mapcar #'leaf-node-value (rest (call-node-args n)))))
    (if (every #'numberp args)
	(let* ((fsym (if f f (leaf-node-value (call-node-function n))))
	       (v (apply (symbol-function fsym) args))
	       (k (call-node-arg n 0)))
	  (move-node-tree
	   (make-continuation-call-node k (make-constant-node v))
	   n)
	  t))))

(defun constant-fold-commutative (n sym ident)
  (let* ((k-args (call-node-args n))
	 (k (first k-args))
	 (args (rest k-args))
	 (nums (remove-if-not #'numberp (mapcar #'leaf-node-value args)))
	 (nnums (remove-if #'(lambda (x) (numberp (leaf-node-value x))) args))
	 (changed nil))
    (case (length nums)
      (0 nil)
      (1 (when (eql (first nums) ident)
	       (set-call-node-args n (cons k nnums))
	       (setf changed t)))
      (t
       (let ((nv (apply sym nums)))
	 (setf args
	       (if (eql nv ident) nnums (cons (make-constant-node nv) nnums)))
	 (set-call-node-args n (cons k args))
	 (setf changed t))))
    (case (length args)
      (0 (move-node-tree
	  (make-continuation-call-node k (make-constant-node ident))
	  n)
	 (setf changed t))
      (1 (move-node-tree (make-continuation-call-node k (first args)) n)
	 (setf changed t)))
    changed))

(define-symbol-call-rule + constant-fold (n)
  (constant-fold-commutative n '+ 0))

(define-symbol-call-rule * constant-fold (n)
  (constant-fold-commutative n '* 1))
     
;;**** add others
(define-symbol-call-rule - constant-fold (n) (constant-fold-numbers n))
(define-symbol-call-rule / constant-fold (n) (constant-fold-numbers n))
(define-symbol-call-rule min constant-fold (n) (constant-fold-numbers n))
(define-symbol-call-rule max constant-fold (n) (constant-fold-numbers n))

(define-symbol-call-rule = constant-fold (n) (constant-fold-numbers n))
(define-symbol-call-rule /= constant-fold (n) (constant-fold-numbers n))
(define-symbol-call-rule < constant-fold (n) (constant-fold-numbers n))
(define-symbol-call-rule > constant-fold (n) (constant-fold-numbers n))
(define-symbol-call-rule <= constant-fold (n) (constant-fold-numbers n))
(define-symbol-call-rule >= constant-fold (n) (constant-fold-numbers n))

(defun change-gfun (n name)
  (set-call-node-function n (get-gfun-node name))
  t)

(define-symbol-call-rule first rename (n) (change-gfun n 'car))
(define-symbol-call-rule second rename (n) (change-gfun n 'cadr))
(define-symbol-call-rule third rename (n) (change-gfun n 'caddr))
(define-symbol-call-rule rest rename (n) (change-gfun n 'cdr))

;;**** assumes result of f1 is last arg of f2
;;**** this is horrible
;;**** need to check for other uses of variable
;;**** think about not immediate use
(defun merge-gfuns (n f2 f12 &optional argc)
  (let ((k (call-node-arg n 0)))
    (if (and (lambda-node-p k)
	     (or (not argc) (= (call-node-arg-count n) (+ argc 1))))
	(let* ((b (lambda-node-body k))
	       (rb (lambda-node-real-body k))
	       (v (first (lambda-node-arglist k)))
	       (f (call-node-function rb)))
	  (if (and (gfun-eq f f2)
		   (eq v (first (last (call-node-args rb))))
		   (= 1 (length (find-references v k))))
	      (let ((nargs (append (butlast (call-node-args rb))
				   (rest (call-node-args n)))))
		(set-call-node-args rb nargs)
		(change-gfun rb f12)
		(move-node-tree b n)
		t))))))

(define-symbol-call-rule car cdr-car=>cdar (n) (merge-gfuns n 'cdr 'cdar))
(define-symbol-call-rule car cdr-car=>caar (n) (merge-gfuns n 'car 'caar))
(define-symbol-call-rule cdr car-cdr=>cadr (n) (merge-gfuns n 'car 'cadr))
(define-symbol-call-rule cdr cdr-cdr=>cddr (n) (merge-gfuns n 'cdr 'cddr))

;;**** more of these??
(define-symbol-call-rule cddr cdr-cddr=>cdddr (n) (merge-gfuns n 'cdr 'cdddr))
(define-symbol-call-rule cdr cddr-cdr=>cdddr (n) (merge-gfuns n 'cddr 'cdddr))
(define-symbol-call-rule cddr car-cddr=>caddr (n) (merge-gfuns n 'car 'caddr))
(define-symbol-call-rule cdr cadr-cdr=>caddr (n) (merge-gfuns n 'cadr 'caddr))

;;
;; constant-fold tests
;;
(define-symbol-call-rule %test constant-fold (n)
  (let ((test (call-node-arg n 2)))
    (if (leaf-node-constant-p test)
	(let ((v (leaf-node-value test))
	      (consequent (call-node-arg n 0))
	      (alternative (call-node-arg n 1)))
	  (if v
	      (move-node-tree (make-null-call-node consequent) n)
	      (move-node-tree (make-null-call-node alternative) n))
	  t))))

(defun make-null-call-node (n)
  (if (lambda-node-p n) (lambda-node-body n) (make-continuation-call-node n)))

;;
;; (if a (if a b c) d) => (if a b d)
;;
(define-symbol-call-rule %test propagate1 (n)
  (let ((test (call-node-arg n 2))
	(consequent (call-node-arg n 0)))
    (if (lambda-node-p consequent)
	(let ((bc (lambda-node-body consequent)))
	  (when (and (test-call-p bc) (var-eq test (call-node-arg bc 2)))
		(insert-node-tree (call-node-arg bc 0) n 1)
		t)))))

;;
;; (if a b (if a c d)) => (if a b d)
;;
(define-symbol-call-rule %test propagate2 (n)
  (let ((test (call-node-arg n 2))
	(alternative (call-node-arg n 1)))
    (if (lambda-node-p alternative)
	(let ((bc (lambda-node-body alternative)))
	  (when (and (test-call-p bc) (var-eq test (call-node-arg bc 2)))
		(insert-node-tree (call-node-arg bc 1) n 2)
		t)))))

#|
;;
;; (lambda () (f)) => f ????
;;
(define-call-rule null-lambda-null-arg-body (n)
  (let ((ch (node-children n))
	(changed nil))
    (dolist (c ch changed)
      (if (and (lambda-node-p c) (null (lambda-node-arglist c)))
	  (let ((b (lambda-node-body c)))
	    (if (null (call-node-args b))
		(let ((f (call-node-function b)))
		  (if (leaf-node-p f)
		      (set-node-children n (subst f c (node-children n)))
		      (move-node-tree f c))
		  (setf changed t))))))))
|#

(defun parse-declarations (decls)
  (macrolet ((add-decl (x d decls)
	       `(let ((e (assoc ,x ,decls)))
		  (if e
		      (setf (cdr e) (cons ,d (cdr e)))
		      (push (list ,x ,d) ,decls)))))
    (let ((vdecls nil)
	  (fdecls nil)
	  (cdecls nil)
	  (dlist (apply #'append (mapcar #'rest decls))))
      (dolist (d dlist)
        (let ((dsym (first d))
	      (dlist (rest d)))
	  (case dsym
	    (optimize (dolist (v dlist) (add-decl dsym v cdecls)))
	    ((inline notinline) (dolist (v dlist) (add-decl v dsym fdecls)))
	    (ftype (dolist (v (rest dlist))
		     (add-decl v (list 'ftype (first dlist)) vdecls)))
	    ((special ignore) (dolist (v dlist) (add-decl v dsym vdecls)))
	    (type (dolist (v (rest dlist))
		    (add-decl v (list 'type (first dlist)) vdecls)))
	    ((array atom bignum bit bit-vector character compiled-function
	      complex cons double-float
	      fixnum float function hash-table integer keyword list
	      long-float nil null number
	      package pathname random-state ratio rational readtable sequence
	      short-float signed-byte simple-array simple-bit-vector
	      simple-string simple-vector single-float standard-char stream
	      string symbol t unsigned-byte vector
	      extended-character real)
	     (dolist (v dlist) (add-decl v (list 'type dsym) vdecls))))))
      (list vdecls fdecls cdecls))))

(defun fixup-let-variables (v)
  (mapcar #'(lambda (x)
	      (cond
	       ((symbolp x) (list x nil))
	       ((and (consp x)
		     (symbolp (first x))
		     (not (consp (rest x))))
		(list (first x) nil))
	       (t x)))  ;**** could do error checking here
	  v))

(defun unparse-variable-declarations (decls)
  (if decls
      (let ((s (first decls)))
	(flet ((unp (d) (if (symbolp d) `(,d ,s) `(,@d ,s))))
         `((declare ,@(mapcar #'unp (rest decls))))))))

(defun unparse-function-declarations (decls)
  (if decls
      (let ((s (first decls)))
	(flet ((unp (d) (if (symbolp d) `(,d ,s) `(,@d ,s))))
         `((declare ,@(mapcar #'unp (rest decls))))))))

(defun unparse-compiler-declarations (decls)
  (if decls `((declare ,@decls))))

(defun declare-no-side-effects (syms)
  (if (symbolp syms)
      (setf (get syms 'cmp-no-side-effects) t)
      (dolist (s syms) (declare-no-side-effects s)))
  nil)

;;**** have to check it is a global function!!!!
(defun no-side-effects-p (s) (get (leaf-node-value s) 'cmp-no-side-effects))

;;**** add others here
(declare-no-side-effects '(+ - * / min max = /= < > <= >= %symval %symfun))

(defun find-y-function (n i)
  (let* ((f (call-node-arg n 0))
	 (b (lambda-node-body f))
	 (bf (call-node-function b)))
    (if (gfun-eq bf '%make-y-closures)
	(setf b (lambda-node-body (call-node-arg b 0))))
    (call-node-arg b (+ i 1))))

(defun find-y-body (n) (find-y-function n -1))

(defun find-y-list-names (n)
  (let* ((p (node-parent n))
	 (pp (node-parent p))
	 (ppf (call-node-function pp)))
    (if (gfun-eq ppf '%make-y-closures)	(setf p (node-parent pp)))
    (lambda-node-arglist p)))

(defun find-lambda-binding (n)
  (if (local-symbol-node-p n)
      (let ((owner (symbol-node-owner n)))
	(if (lambda-node-p owner)
	    (let ((p (node-parent owner)))
	      (cond
	       ((and (call-node-p p) (eq owner (first (node-children p))))
		(let* ((pos (position n (lambda-node-arglist owner)))
		       (val (call-node-arg p pos)))
		  (find-lambda-binding val)))
	       ((and (call-node-p p)   ;;**** is this right now???
		     (gfun-member (call-node-function p)
				  '(%y %make-y-closures)))
		(let ((i (position n (lambda-node-arglist owner))))
		  (find-y-function p i)))))))
      n))

(defun find-lambda-variable (f)
  (let* ((p (node-parent f))
	 (g (call-node-function p)))
    (cond
     ((lambda-node-p g)
      (if (not (eq f g))
	  (nth (position f (call-node-args p)) (lambda-node-arglist g))))
     ((gfun-eq g '%y-list)
      (nth (- (position f (call-node-args p)) 1) (find-y-list-names p))))))

(defun unsafe-continuation-lambda-p (form parent)
  (and (lambda-node-p form)
       (not (continuation-node-p form))
       (/= (position form (node-children parent)) 0)))

(defun safe-continuation-reference-p (r)
  (let* ((form (car r))
	 (i (cdr r))
	 (k (call-node-arg form 0))
	 (owner (symbol-node-owner k)))
    (do* ((f form p)
	  (p (node-parent f) (node-parent f)))
	 ((eq f owner) t)
	 (if (unsafe-continuation-lambda-p f p) (return nil)))))

(define-symbol-call-rule %catch-block skip-catch-if-safe (n)
  (let* ((k (call-node-arg n 0))
	 (b (call-node-arg n 2))
	 (fk (first (lambda-node-arglist b)))
	 (tag (second (lambda-node-arglist b)))
	 (refs (find-references tag (lambda-node-body b))))
    (when (or (null refs) (every #'safe-continuation-reference-p refs))
	  ;;**** The %throw-return-from must be in a multiple value
	  ;;**** continuation. This replaces the multiple value continuation
	  ;;**** where it occurs (which must be in continuation position)
	  ;;**** by fk. This needs to be revised if the representation of
	  ;;**** multiple value continuations changes.
	  ;;**** there may be a simpler way to do the replacement
	  (flet ((mvform (n)
		   (node-parent (node-parent (node-parent (node-parent n))))))
	    (dolist (r refs)
	      (let* ((form (car r))
		     (m (mvform form))
		     (mf (call-node-function m))
		     (margs (rest (call-node-args m))))
		(move-node-tree (apply #'make-call-node mf fk margs) m))))
	  (lambda-node-delete-arg b tag)
	  (move-node-tree (make-call-node b k) n)
	  t)))


;;
;; Y combinator simplifiers
;;
;;**** check this over carefully !!!
(define-symbol-call-rule %y hoist-non-recursives (n)
  (let* ((yf (call-node-arg n 0))
	 (yvars (copy-list (lambda-node-arglist yf)))
	 (ybody (lambda-node-body yf))
	 (yfuns (copy-list (rest (call-node-args ybody))))
	 (pos nil))
    (flet ((not-used (x y)
	     (every #'(lambda (z) (not (any-references-p x z))) y)))
    (dolist (v yvars) (if (not-used v yfuns) (push (position v yvars) pos)))
    (when pos
	  (let ((dvars nil)
		(dfuns nil))
	    (dolist (p pos)
	      (let ((v (nth p yvars))
		    (f (nth p yfuns)))
		(lambda-node-delete-arg yf v)
		(call-node-delete-arg ybody f) ;;**** fix
		(push v dvars)
		(push f dfuns)))
	    (let ((m (lambda-node-body (call-node-arg ybody 0))))
	      (move-node-tree
	       (apply #'make-call-node
		      (make-lambda-node dvars (copy-node-tree m))
		      dfuns)
	       m)))
	  t))))

(define-symbol-call-rule %y no-functions (n)
  (let ((ybody (lambda-node-body (call-node-arg n 0))))
    (if (= (call-node-arg-count ybody) 1)
	(let ((b (lambda-node-body (call-node-arg ybody 0))))
	  (move-node-tree b n)
	  t))))

;;***** think this stuff through!!!
(defun safe-go-path-p (form y)
  (do ((f form (node-parent f)))
      ((eq f y) t)
      (if (and (lambda-node-p f)
	       (not (continuation-node-p f))
	       (/= (position f (node-children (node-parent f))) 0))
	  (return nil))))

(defun safe-go-references-p (refs body)
  (dolist (r refs t)
    (unless (or (gfun-eq (call-node-function (car r)) '%do-catch-tagbody)
		(safe-go-path-p (car r) body))
	    (return nil))))

(define-symbol-call-rule %catch-tagbody skip-tagbody-catch (n)
  (let* ((fun (call-node-arg n 1))
	 (body (lambda-node-body fun))
	 (tag (second (lambda-node-arglist fun)))
	 (refs (find-references tag body)))
    (when (safe-go-references-p refs body)
	  (dolist (r refs)
	    ;; this should work for %throw-go and %do-catch-tagbody references
	    (move-node-tree (make-call-node (call-node-arg (car r) 1))
			    (car r)))
	  (lambda-node-delete-arg fun tag)
	  (move-node-tree (make-call-node fun (call-node-arg n 0)) n)
	  t)))

;;**** think these through
;; (funcall #'gfun ...) => (gfun ...)
(define-symbol-call-rule %symfun funcall-symfun-gfun-to-call (n)
  (let ((k (call-node-arg n 0)))
    (if (lambda-node-p k)
	(let* ((v (first (lambda-node-arglist k)))
	       (b (lambda-node-body k))
	       (f (call-node-function b)))
	  (when (and (gfun-eq f 'funcall)
		     (eq v (call-node-arg b 1)))
		(move-node-tree (apply #'make-call-node
				       (call-node-arg n 1)
				       (call-node-arg b 0)
				       (rest (rest (call-node-args b))))
				n)
		t)))))

;; (funcall fun ...) => (fun ...)
;;**** fixed to use lambda binding
;;**** probably needs to be done other places
;;**** need some utilities to help
#|
(define-symbol-call-rule funcall funcall-function-to-call (n)
  (let ((f (call-node-arg n 1)))
    (when (or (lambda-node-p f) (gfun-node-p f) (lfun-node-p f))
	  (call-node-delete-arg-index n 1)
	  (set-call-node-function n f)
	  t)))
|#
(define-symbol-call-rule funcall funcall-function-to-call (n)
  (let ((f (call-node-arg n 1)))
    (when (or (lambda-node-p f) (gfun-node-p f) (lfun-node-p f))
	  (with-saved-cmp-environments
	   (let ((fv (new-fenv-node (gensym "F")))
		 (k (call-node-arg n 0))
		 (f (call-node-arg n 1))
		 (args (rest (rest (call-node-args n)))))
	     (move-node-tree
	      (make-call-node
	       (make-lambda-node (list fv) (apply #'make-call-node fv k args))
	       f)
	      n)))
	  t)))

(define-symbol-call-rule funcall quote-symbol-to-gfun (n)
  (let ((f (call-node-arg n 1)))
    (when (constant-node-p f)
	  (let ((qfs (leaf-node-value f)))
	    (unless (and (consp qfs)
			 (eq (first qfs) 'quote)
			 (symbolp (second qfs)))
		    (error "bad FUNCALL argument -- ~s" qfs))
	    (set-call-node-arg n 1 (get-gfun-node (second qfs)))
	    t))))

(define-symbol-call-rule %mvc quote-symbol-to-gfun (n)
  (let ((f (call-node-arg n 1)))
    (when (constant-node-p f)
	  (let ((qfs (leaf-node-value f)))
	    (unless (and (consp qfs)
			 (eq (first qfs) 'quote)
			 (symbolp (second qfs)))
		    (error "bad MULTIPLE-VALUE-CALL argument -- ~s" qfs))
	    (set-call-node-arg n 1 (get-gfun-node (second qfs)))
	    t))))

;;**** fix if used rest args are allowed
(define-symbol-call-rule %mvc lambda-to-continuation (n)
  (let* ((f (call-node-arg n 1))
	 (rest (nth 5 (lambda-node-lambda-list f))))
    (when (and (lambda-node-p f)
	       (simple-lambda-node-p f))
	  (with-saved-cmp-environments
	   (let* ((k (call-node-arg n 0))
		  (args (rest (lambda-node-arglist f)))
		  (uargs (if rest (butlast args) args))
		  (asyms (mapcar #'leaf-node-value uargs))
		  (targs (mapcar #'new-env-node asyms)))
	     (move-node-tree
	      (make-call-node
	       (get-gfun-node '%mvcc)
	       (make-continuation-node
		targs
		(apply #'make-call-node f k targs)))
	      n)
	     t)))))

(defun non-tail-continuation-p (k) (lambda-node-p k))

;;**** might need to be redone
(defun multiple-value-continuation-node-p (c)
  (and (lambda-node-p c)
       (gfun-eq (call-node-function (lambda-node-body c)) '%mv-collect)))
	      
(define-symbol-call-rule values-list non-tail-call (n)
  (let ((k (find-lambda-binding (call-node-arg n 0))))
    (if (and (non-tail-continuation-p k)
	     (not (multiple-value-continuation-node-p k)))
      (change-gfun n 'car))))

(define-symbol-call-rule values non-tail-call (n)
  (let* ((karg (call-node-arg n 0))
	 (k (find-lambda-binding karg)))
    (if (and (non-tail-continuation-p k)
	     (not (multiple-value-continuation-node-p k)))
	(let ((a (if (= (call-node-arg-count n) 1)
		     (make-constant-node nil)
		     (call-node-arg n 1))))
	  (move-node-tree (make-continuation-call-node karg a) n)
	  t))))

(define-symbol-call-rule values one-value (n)
  (when (and (= (call-node-arg-count n) 2)
	     (not (multiple-value-continuation-node-p (call-node-arg n 0))))
	(move-node-tree
	 (make-continuation-call-node (call-node-arg n 0) (call-node-arg n 1))
	 n)
	t))

;;**** %mvc %mvcc simplifiers for (values ...) form?
;; (multiple-value-call #'(lambda (x) (+ x 1)) y) => (+ y 1)
;; (multiple-value-call #'(lambda (x y) (+ x y)) (values u v)) => (+ u v)

;;**** this stuff should be incorporated in rules
;;**** that means changing the representation of %test expressions
#|
(defun merge-tests (n)
  (dolist (c (node-children n)) (merge-tests c))
  (if (call-node-p n)
      (let ((f (call-node-function n)))
	(if (gfun-node-p f)
	    (case (gfun-symbol f)
	     (= (merge-gfuns n '%test '%test= 2))
	     (/= (merge-gfuns n '%test '%test/= 2))
	     (< (merge-gfuns n '%test '%test< 2))
	     (> (merge-gfuns n '%test '%test> 2))
	     (>= (merge-gfuns n '%test '%test>= 2))
	     (<= (merge-gfuns n '%test '%test<= 2))
	     (consp (merge-gfuns n '%test '%test-consp 1))
	     (endp (merge-gfuns n '%test '%test-endp 1))
	     (eq (merge-gfuns n '%test '%test-eq 2))
	     (eql (merge-gfuns n '%test '%test-eql 2))
	     (equal (merge-gfuns n '%test '%test-equal 2))
	     (%supplied-p (merge-gfuns n '%test '%test-supplied-p 1))))))
  n)
|#
;;**** this is an iterative version to avoid argument stack overflows
(defun merge-tests (n)
  (let ((todo (cond
	       ((lambda-node-p n) (list (cons (lambda-node-body n) nil)))
	       ((call-node-p n) (list (cons n nil)))
	       (t nil))))	       
    (loop
     (cond
      ((null todo) (return n))
      ((null (cdr (first todo)))
       (setf (cdr (first todo)) t)
       (let ((c (car (first todo))))
	 (dolist (x (node-children c))
	   (cond
	    ((lambda-node-p x) (push (cons (lambda-node-body x) nil) todo))
	    ((call-node-p x) (push (cons x nil) todo))))))
      (t (let ((c (car (pop todo))))
	   (unless (call-node-p c) (error "non call node in merge - ~s" c))
	   (let ((f (call-node-function c)))
	     (if (gfun-node-p f)
		 (case (gfun-symbol f)
		   (= (merge-gfuns c '%test '%test= 2))
		   (/= (merge-gfuns c '%test '%test/= 2))
		   (< (merge-gfuns c '%test '%test< 2))
		   (> (merge-gfuns c '%test '%test> 2))
		   (>= (merge-gfuns c '%test '%test>= 2))
		   (<= (merge-gfuns c '%test '%test<= 2))
		   (consp (merge-gfuns c '%test '%test-consp 1))
		   (endp (merge-gfuns c '%test '%test-endp 1))
		   (eq (merge-gfuns c '%test '%test-eq 2))
		   (eql (merge-gfuns c '%test '%test-eql 2))
		   (equal (merge-gfuns c '%test '%test-equal 2))
		   (%supplied-p (merge-gfuns c
					     '%test
					     '%test-supplied-p 1)))))))))))

