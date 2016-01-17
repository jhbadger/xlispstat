;;**** allow simple lambdas t have used rest arguments?
;;**** rethink substitution of multiple value continuations

(in-package "XLSCMP")

;;;;;
;;;;; Macro expansion, alphatization, and CPS Conversion
;;;;;
#|

Special forms done (sort of):

BLOCK CASE CATCH ERRSET FLET FUNCTION GO IF LABELS LAMBDA LET LET*
LOOP PROGN PROGV QUOTE RETURN-FROM SETQ TAGBODY THROW UNWIND-PROTECT
LOCALLY MULTIPLE-VALUE-CALL MULTIPLE-VALUE-PROG1 DECLARE


Special forms needed later:

GENERIC-FLET GENERIC-LABELS LOAD-TIME-VALUE WITH-ADDED-METHODS
|#


;;;;
;;;; Internal CPS node representation
;;;;

(setf (symbol-function 'leaf-node-p) #'cps-leaf-node-p)
(setf (symbol-function 'lambda-node-p) #'cps-lambda-node-p)
(setf (symbol-function 'call-node-p) #'cps-call-node-p)

(setf (symbol-function 'node-children) #'cps-node-children)
(setf (symbol-function 'node-parent) #'cps-node-parent)
(setf (symbol-function 'node-simplified-p) #'cps-node-simplified-p)
(setf (symbol-function 'node-note) #'cps-node-note)

(setf (symbol-function 'set-node-children) #'cps-set-node-children)
(setf (symbol-function 'set-node-parent) #'cps-set-node-parent)
(setf (symbol-function 'set-node-simplified) #'cps-set-node-simplified)
(setf (symbol-function 'set-node-note) #'cps-set-node-note)

(setf (symbol-function 'leaf-node-value) #'cps-leaf-node-value)
(setf (symbol-function 'leaf-node-count) #'cps-leaf-node-count)

(setf (symbol-function 'set-leaf-node-value) #'cps-set-leaf-node-value)
(setf (symbol-function 'set-leaf-node-count) #'cps-set-leaf-node-count)

(setf (symbol-function 'lambda-node-arglist) #'cps-lambda-node-arglist)
(setf (symbol-function 'lambda-node-lambda-list) #'cps-lambda-node-lambda-list)
(setf (symbol-function 'lambda-node-name) #'cps-lambda-node-name)

(setf (symbol-function 'set-lambda-node-arglist) #'cps-set-lambda-node-arglist)
(setf (symbol-function 'set-lambda-node-lambda-list) #'cps-set-lambda-node-lambda-list)
(setf (symbol-function 'set-lambda-node-name) #'cps-set-lambda-node-name)

(setf (symbol-function 'lambda-node-body) #'cps-lambda-node-body)
(setf (symbol-function 'call-node-function) #'cps-call-node-function)
(setf (symbol-function 'call-node-args) #'cps-call-node-args)

(setf (symbol-function 'any-references-p) #'cps-any-references-p)
(setf (symbol-function 'find-references) #'cps-find-references)


;;;;;
;;;;; Additions to internal CPS node representation
;;;;;

;; general nodes
(defun make-node () (make-cps-node nil))

(defun node-expression (e)
  (cond
   ((leaf-node-p e) (leaf-node-value e))
   ((lambda-node-p e)
    `(,(if (continuation-node-p e) 'continuation 'lambda)
      ,(mapcar #'leaf-node-value (lambda-node-arglist e))
      ,(node-expression (first (node-children e)))))
   ((call-node-p e) (mapcar #'node-expression (node-children e)))))

;; leaf nodes
(defun make-leaf-node (v)
  (let ((n (make-cps-node 'leaf)))
    (set-node-simplified n t)
    (set-leaf-node-value n v)
    n))

;; lambda nodes
(defun make-lambda-node (arglist body &optional lambda-list)
  (let ((n (make-cps-node 'lambda)))
    (set-lambda-node-arglist n arglist)
    (set-node-children n (list body))
    (dolist (a arglist) (set-symbol-node-owner a n))
    (set-node-parent body n)
    (set-lambda-node-lambda-list
     n
     (if lambda-list lambda-list (lambda-list-default arglist)))
    n))
 
(defun lambda-node-delete-arg (n v)
  (set-lambda-node-arglist n (delete v (lambda-node-arglist n)))
  (set-lambda-node-lambda-list n
			       (lambda-list-default (lambda-node-arglist n))))
;;**** check use of this
(defun set-lambda-node-arglist-fix (n a)
  (set-lambda-node-arglist n a)
  (set-lambda-node-lambda-list n (lambda-list-default a)))
  
;; call nodes
(defun make-call-node (&rest args)
  (let ((n (make-cps-node 'call)))
    (set-node-children n args)
    (dolist (c args) (set-node-parent c n))
    (if (lambda-node-p (first args)) (fixup-lambda-call-node n))
    n))

(defun call-node-add-arg (n a)
  (set-node-children n (append (node-children n) (list a)))
  n)

(defun call-node-arg (n k) (nth k (call-node-args n)))

(defun call-node-arg-count (n) (length (call-node-args n)))

;;**** this needs to be replaced completely by the index based one
(defun call-node-delete-arg (n v)
  (set-node-children n (delete v (node-children n)))
  (set-node-parent v nil))

;;**** this is a hack -- it should be done more efficiently
(defun call-node-delete-arg-index (n i)
  (let* ((c (node-children n))
	 (v (nth (+ i 1) c))
	 (x (cons nil nil))) ;;**** a unique marker
    (setf (nth (+ i 1) c) x)
    (set-node-children n (delete x c))
    (set-node-parent v nil)))

(defun set-call-node-args (n a)
  (dolist (c a) (unless (leaf-node-p c) (set-node-parent c n)))
  (set-node-children n (cons (first (node-children n)) a))
  (fixup-lambda-call-node n))

(defun set-call-node-function (n f)
  (unless (leaf-node-p f) (set-node-parent f n))
  (set-node-children n (cons f (rest (node-children n))))
  (fixup-lambda-call-node n))

(defun set-call-node-arg (n i a)
  (unless (leaf-node-p a) (set-node-parent a n))
  (setf (nth (+ i 1) (node-children n)) a)
  (fixup-lambda-call-node n))

;; node moving and copying
(defun copy-node-internals (b a)
  (if (leaf-node-p b) (error "can't copy a leaf node"))
  (let ((pa (node-parent a)))
    (cps-node-transform a b)
    (set-node-parent a pa)))

(defun move-node-tree (b a)
  (copy-node-internals b a)
  (dolist (c (node-children a)) (set-node-parent c a))
  (do ((p (node-parent a) (node-parent p)))
      ((not (and p (node-simplified-p p))))
      (set-node-simplified p nil)))

(defun internal-insert-node-tree (tree form pos)
  (setf (nth pos (node-children form)) tree)
  (unless (leaf-node-p tree) (set-node-parent tree form)))

(defun insert-node-tree (tree form pos)
  (internal-insert-node-tree tree form pos)
  (do ((p form (node-parent p)))
      ((not (and p (node-simplified-p p))))
      (set-node-simplified p nil)))

;;****** this may be dangerous as it does not replace variable nodes
;;****** variable node replacement is needed for multiple inlining
(defun copy-node-tree (tree)
  (if (leaf-node-p tree)
      tree
      (let ((cp (make-node)))
	(copy-node-internals tree cp)
	(if (lambda-node-p cp)
	    (dolist (a (lambda-node-arglist cp))
	      (set-symbol-node-owner a cp)))
	(let ((ch (copy-list (node-children cp))))
	  (set-node-children cp ch)
	  (dotimes (i (length ch))
	    (internal-insert-node-tree (copy-node-tree (nth i ch)) cp i))
	  cp))))

;; continuation nodes
(defun continuation-node-p (n)
  (and (lambda-node-p n) (eq (node-note n) 'continuation)))

(defun make-continuation-node (arglist body)
  (let ((n (make-lambda-node arglist body)))
    (set-node-note n 'continuation)
    n))

#|
(setf count 0)
(defun mark-tree-unsimplified (n)
  (incf count)
  (unless (leaf-node-p n) (set-node-simplified n nil))
  (dolist (c (node-children n)) (mark-tree-unsimplified c)))
|#

;;;;
;;;; Representation-independent part
;;;;

(defun pp-cps (e) (pprint (node-expression e)))

;;;
;;; Conversion rules management
;;;
(defun set-cps-converter (s c) (setf (get s 'cps-converter) c))

;;***** check for local function binding
(defun get-cps-converter (s)
  (if (and (symbolp s) (null (assoc s *cmp-fenv*)))
      (get s 'cps-converter)))

(defmacro define-cps-converter (sym &rest body)
  `(set-cps-converter ',sym #'(lambda ,@body)))

;;;
;;; CPS converter
;;;

(defvar *cmp-env* nil)
(defvar *cmp-fenv* nil)
(defvar *cmp-denv* nil)
(defvar *cmp-tenv* nil)
(defvar *cmp-specials* nil)
(defvar *cmp-macros* nil)
(defvar *cmp-setf* nil)
(defvar *cmp-structs* nil)
(defvar *cmp-gvars* nil)
(defvar *cmp-gfuns* nil)
(defvar *cmp-consts* nil)
(defvar *cmp-base-continuation-symbol* (gensym "K0-"))

(defun lfun-node-p (n) (lambda-node-p (find-lambda-binding n)))

(defun gfun-node-p (n) (and (leaf-node-p n) (eq (symbol-node-owner n) 'gfun)))
(defun gvar-node-p (n) (and (leaf-node-p n) (eq (symbol-node-owner n) 'gvar)))
(defun constant-node-p (n) (and (leaf-node-p n) (eq (node-note n) 'constant)))

(defun gfun-eq (n s) (and (gfun-node-p n) (eq (gfun-symbol n) s)))
(defun gfun-member (n sl) (and (gfun-node-p n) (member (gfun-symbol n) sl)))
(defun gfun-symbol (n) (leaf-node-value n))

(defun local-symbol-node-p (n)
  (and (leaf-node-p n) (lambda-node-p (symbol-node-owner n))))

(defun make-variable-node (e) (make-leaf-node e))
(defun make-function-node (e) (make-leaf-node e))

(defun make-constant-node (e)
  (let ((n (make-leaf-node e)))
    (set-node-note n 'constant)
    n))

(defun symbol-node-owner (n) (node-note n))
(defun set-symbol-node-owner (n o) (set-node-note n o))

(defun new-env-node (sym)
  (let ((n (make-variable-node sym)))
    (push (list sym n) *cmp-env*)
    n))

(defun new-fenv-node (sym)
  (let ((n (make-function-node sym)))
    (push (list sym n) *cmp-fenv*)
    n))

(defun make-gvar-node (sym)
  (let ((n (make-variable-node sym)))
    (set-symbol-node-owner n 'gvar)
    n))

(defun make-gfun-node (sym)
  (let ((n (make-function-node sym)))
    (set-symbol-node-owner n 'gfun)
    n))

(defun new-gvar-node (sym)
  (let ((n (make-gvar-node sym)))
    (push (list sym n) *cmp-gvars*)
    n))

(defun new-gfun-node (sym)
  (let ((n (make-gfun-node sym)))
    (push (list sym n) *cmp-gfuns*)
    n))

(defun new-constant-node (e)
  (let ((n (make-constant-node e)))
    (push (list e n) *cmp-consts*)
    n))

(defun get-gvar-node (sym)
  (let ((gnode (second (assoc sym *cmp-gvars*))))
    (if gnode gnode (new-gvar-node sym))))

(defun get-gfun-node (sym)
  (let ((gnode (second (assoc sym *cmp-gfuns*))))
    (if gnode gnode (new-gfun-node sym))))

(defun get-variable-node (sym)
  (if (is-special-variable sym)
      (get-gvar-node sym)
      (let ((lnode (second (assoc sym *cmp-env*))))
	(if lnode lnode (get-gvar-node sym)))))
  
(defun get-function-node (sym)
  (let ((lnode (second (assoc sym *cmp-fenv*))))
    (if lnode lnode (get-gfun-node sym))))

(defun get-constant-node (e)
  (let ((cnode (second (assoc e *cmp-consts* :test #'equal))))
    (if cnode cnode (new-constant-node e))))

(defun is-special-variable (sym)
  (or (specialp sym) (member sym *cmp-specials*)))

(defmacro with-saved-cmp-environments (&rest body)
  `(progv '(*cmp-env* *cmp-fenv* *cmp-denv* *cmp-tenv* *cmp-specials*)
	  (list *cmp-env* *cmp-fenv* *cmp-denv* *cmp-tenv* *cmp-specials*)
	  ,@body))

(defun cvt (e)
  (progv '(*cmp-env*   *cmp-fenv*  *cmp-denv*   *cmp-tenv* *cmp-specials*
	   *cmp-gvars* *cmp-gfuns* *cmp-consts*)
	 '(nil nil nil nil nil nil nil nil)
	 (let ((k0 (get-function-node *cmp-base-continuation-symbol*)))
	   (convert e k0))))

;;**** fix to use intrnal constant status
(defun constant-symbol-p (e) (member e '(t nil)))

(defun constant-value-form (e)
  (let ((v (if e (symbol-value e) nil)))
     (if (or (null v) (eq t v) (characterp v) (numberp v)) v `(quote ,v))))

(defun make-continuation-call-node (k &rest args)
  (if (multiple-value-continuation-node-p k)
      (apply #'make-call-node (get-gfun-node 'values) k args)
      (apply #'make-call-node k args)))

#|
(defun convert (e k)
  (setf e (cmp-macroexpand e))
  (cond 
   ((or (atom e) (eq (first e) 'quote))
    (if (symbolp e)
	(if (constant-symbol-p e)
	    (make-continuation-call-node
	     k
	     (get-constant-node (constant-value-form e)))
	    (let ((v (get-variable-node e)))
	      (if (gvar-node-p v)
		  (convert `(%symval ',e) k)
		  (make-continuation-call-node k v))))
        (make-continuation-call-node k (get-constant-node e))))
   (t (check-arg-count e)
      (let ((c (get-cps-converter (first e))))
	(if c
	    (funcall c e k) 
	    (convert-funarg
	     (first e) 
	     (with-saved-cmp-environments
	      (let ((p (new-env-node (gensym "P"))))
		(make-continuation-node
		 (list p)
		 (convert-arguments (rest e)
				    (make-call-node p k)))))))))))
|#
;;***** includes initial variable substitution
;;***** make sure this initial substitution is OK
(defun pre-simplify-tree (n)
  (when (and (call-node-p n) (lambda-node-p (call-node-function n)))
	(lambda-call-substitute-variables n nil t)
	(let ((f (call-node-function n)))
	  (if (null (lambda-node-arglist f))
	      (move-node-tree (lambda-node-body f) n)))))

(defun convert (e k)
  (flet ((convert1 (e k)
           (setf e (cmp-macroexpand e))
	   (cond 
	    ((or (atom e) (eq (first e) 'quote))
	     (if (symbolp e)
		 (if (constant-symbol-p e)
		     (make-continuation-call-node
		      k
		      (get-constant-node (constant-value-form e)))
		   (let ((v (get-variable-node e)))
		     (cond
		      ((gvar-node-p v)
		       (unless (or (is-special-variable e)
				   (not *compile-warn-specials*))
			       (warn "variable ~s assumed special" e))
		       (convert `(%symval ',e) k));;////// warn if not special
		      (t (make-continuation-call-node k v)))))
	       (make-continuation-call-node k (get-constant-node e))))
	    (t (check-arg-count e)
	       (let ((c (get-cps-converter (first e))))
		 (if c
		     (funcall c e k) 
		   (convert-funarg
		    (first e) 
		    (with-saved-cmp-environments
		     (let ((p (new-env-node (gensym "P"))))
		       (make-continuation-node
			(list p)
			(convert-arguments (rest e)
					   (make-call-node p k))))))))))))
    (let ((n (convert1 e k)))
      (pre-simplify-tree n)
      n)))

;;**** simplify symbol case?
(defun convert-funarg (f k)
  (cond
   ((symbolp f)
    (make-call-node k (get-function-node f)))
   ((and (consp f) (eq (first f) 'lambda)) (convert f k))
   (t (error "bad function - ~s" f))))

#|
;****//// this causes deep recursions
(defun convert-arguments (args final-call)
  (if (null args)
      final-call
      (with-saved-cmp-environments
        (let ((k (new-env-node (gensym "K"))))
	  (convert (car args)
		   (make-continuation-node
		    (list k)
		    (convert-arguments (rest args)
				       (call-node-add-arg final-call k))))))))
|#
;;****** iterative version
(defun convert-arguments (args final-call)
  (let ((vars nil))
    (with-saved-cmp-environments
     (dolist (a args) (push (new-env-node (gensym "V")) vars)))
    ;; because of substitution, adding the arguments MUST come first
    (set-node-children final-call
		       (append (node-children final-call) (reverse vars)))
    (do ((form final-call)
	 (rargs (reverse args) (rest rargs))
	 (vars vars (rest vars)))
	((null rargs) form)
	(let ((a (first rargs))
	      (v (first vars)))
	  (setf form (convert a (make-continuation-node (list v) form)))))))


;;;
;;; Specific conversion rules
;;;

(defun fixup-optional-argument (x)
  (cond
   ((symbolp x) (list x nil nil))
   ((and (consp x) (symbolp (first x)))
    (check-supplied-p-argument (third x))
    (list (first x) (cmp-macroexpand (second x)) (third x)))
   (t (error "bad optional argument entry -- ~s" x))))

(defun make-corresponding-keyword (s) (intern (symbol-name s) "KEYWORD"))

(defun check-supplied-p-argument (s)
  (if s (unless (symbolp s) (error "bad supplied-p argument -- ~s" s))))

(defun fixup-keyword-argument (x)
  (cond
   ((symbolp x) (list (make-corresponding-keyword x) x nil nil))
   ((and (consp x) (symbolp (first x)))
    (check-supplied-p-argument (third x))
    (list (make-corresponding-keyword (first x))
	  (first x)
	  (cmp-macroexpand (second x))
	  (third x)))
   ((and (consp x)
	 (consp (first x))
	 (symbolp (first (first x)))
	 (symbolp (second (first x)))
	 (null (rest (rest (first x)))))
    (check-supplied-p-argument (third x))
    (list (first (first x))
	  (second (first x)) 
	  (cmp-macroexpand (second x))
	  (third x)))
   (t (error "bad keyword argument entry -- ~s" x))))

(defun fixup-aux-argument (x)
  (cond
   ((symbolp x) (list x nil))
   ((and (consp x) (symbolp (first x))) (list (first x) (second x)))
   (t (error "bad aux argument entry -- ~s" x))))

(defun is-lambda-key (s) (member s lambda-list-keywords))

;;**** put in better error checking here
(defun split-lambda-list (ll)
  (let ((env (second (member '&environment ll)))
	(req nil)
	(opt nil)
	(rest nil)
	(allow-keys nil)
	(key nil)
	(allow-other-keys nil)
	(aux nil))
    (if env (setf ll (remove '&environment (remove env ll))))
    (when ll
	  (loop
	   (let ((x (first ll)))
	     (if (is-lambda-key x) (return))
	     ;;(assert (symbolp x))
	     (push x req)
	     (setf ll (rest ll))
	     (if (null ll) (return)))))
    (when (eq (first ll) '&optional)
	  (setf ll (rest ll))
	  (loop
	   (let ((x (first ll)))
	     (if (or (null ll) (is-lambda-key x)) (return))
	     (push (fixup-optional-argument x) opt)
	     (setf ll (rest ll)))))
    (when (eq (first ll) '&rest)
	  (setf ll (rest ll))
	  ;;(assert (and (consp ll) (symbolp (first ll))))
	  (setf rest (first ll))
	  (setf ll (rest ll)))
    (when (eq (first ll) '&key)
	  (setf ll (rest ll))
	  (setf allow-keys t)
	  (loop
	   (let ((x (first ll)))
	     (if (or (null ll) (is-lambda-key x)) (return))
	     (push (fixup-keyword-argument x) key)
	     (setf ll (rest ll)))))
    (when (eq (first ll) '&allow-other-keys)
	  (setf ll (rest ll))
	  (setf allow-other-keys t))
    (when (eq (first ll) '&aux)
	  (setf ll (rest ll))
	  (loop
	   (let ((x (first ll)))
	     (if (or (null ll) (is-lambda-key x)) (return))
	     (push (fixup-aux-argument x) aux)
	     (setf ll (rest ll)))))
    (unless (null ll) (error "bad formal argument list"))
    (list (reverse req)
	  (reverse opt)
	  rest
	  allow-keys
	  (reverse key)
	  (reverse aux)
	  allow-other-keys
	  env)))

(defun rewrite-lambda (lambda-list body)
  (let* ((sl (split-lambda-list lambda-list))
	 (req (nth 0 sl))
	 (opt (nth 1 sl))
	 (rest (nth 2 sl))
	 (allow-keys (nth 3 sl))
	 (key (nth 4 sl))
	 (aux (nth 5 sl))
	 (allow-other-keys (nth 6 sl))
	 (env (nth 7 sl))
	 (db (split-declarations body))
	 (pdcl (parse-declarations (first db)))
	 (rdcl (mapcar #'(lambda (x) (assoc x (first pdcl))) req))
	 (odcl (set-difference (first pdcl) rdcl))
	 (body (second db))
	 (key-defaults nil)
	 (opt-defaults nil)
	 (variables nil)
	 (bindings aux))
    (if env
	(let ((ee (gensym "E")))
	  (push `(,env ,ee) bindings)
	  (push ee variables)))
    (dolist (v (reverse key))
      (let ((s (second v))
	    (ss (gensym "K"))
	    (dflt (third v))
	    (sp (fourth v)))
	(push ss variables)
	(cond
	 ((and (constant-expression-p dflt) (null sp))
	  (push (strip-quote dflt) key-defaults)
	  (push `(,s ,ss) bindings))
	 (t
	  (push '%not-supplied key-defaults)
	  (push `(,s (if (%supplied-p ,ss) ,ss ,dflt)) bindings)
	  (if sp (push `(,sp (%supplied-p ,ss)) bindings))))))
    (if rest
	(let ((rr (gensym "R")))
	  (push `(,rest ,rr) bindings)
	  (push rr variables)))
    (dolist (v (reverse opt))
      (let ((s (first v))
	    (ss (gensym "O"))
	    (dflt (second v))
	    (sp (third v)))
	(push ss variables)
	(cond
	 ((and (constant-expression-p dflt) (null sp))
	  (push (strip-quote dflt) opt-defaults)
	  (push `(,s ,ss) bindings))
	 (t
	  (push '%not-supplied opt-defaults)
	  (push `(,s (if (%supplied-p ,ss) ,ss ,dflt)) bindings)
	  (if sp (push `(,sp (%supplied-p ,ss)) bindings))))))
    (dolist (v (reverse req)) (push v variables))
    (list variables
	  (if bindings
	      `(,@(apply #'append
			 (mapcar #'unparse-variable-declarations rdcl))
		(let* ,bindings
		  ,@(apply #'append 
			   (mapcar #'unparse-variable-declarations odcl))
		  ,@(unparse-function-declarations (second pdcl))
		  ,@(unparse-compiler-declarations (third pdcl))
		  ,@body))
	      `(,@(apply #'append
			 (mapcar #'unparse-variable-declarations rdcl))
		,@(apply #'append 
			 (mapcar #'unparse-variable-declarations odcl))
		,@(unparse-function-declarations (second pdcl))
		,@(unparse-compiler-declarations (third pdcl))
		,@body))
	  (length req)
	  (length opt)
	  opt-defaults
	  allow-keys
	  key-defaults
	  rest
	  allow-other-keys
	  (mapcar #'first key)
	  env)))

(defun lambda-list-default (args)
  (append (list (length args) 0) (make-list 6)))

(defun lambda-list-data (args)
  (let* ((sl (split-lambda-list args))
	 (req (nth 0 sl))
	 (opt (nth 1 sl))
	 (rest (nth 2 sl))
	 (allow-keys (nth 3 sl))
	 (key (nth 4 sl))
	 (aux (nth 5 sl))
	 (allow-other-keys (nth 6 sl))
	 (env (nth 7 sl)))
    (list (length req)
	  (length opt)
	  (mapcar #'second opt)
	  allow-keys
	  (mapcar #'third key)
	  rest
	  allow-other-keys
	  (mapcar #'first key)
	  env)))

(defun find-lambda-list-data (f)
  (if (and (consp f) (eq (first f) 'lambda))
      (lambda-list-data (second f))
      (let ((lf (second (assoc f *cmp-fenv*))))
	(if lf
	    nil;;(lambda-node-lambda-list (find-lambda-binding lf));;////????
	    (get-lambda-list-data f)))))

(defmacro define-lambda-list (s ll)
  `(setf (get ',s 'cmp-lambda-list-data) (lambda-list-data ',ll)))

(defun get-lambda-list-data (s) (get s 'cmp-lambda-list-data))

;;****** need lots more of this
;;****** may also need for setf methods, eg
;;(define-lambda-list (setf aref) (a i &rest args))
(define-lambda-list car (x))
(define-lambda-list aref (a i &rest args))
(define-lambda-list %set-aref (x y z &rest args)) ;;**** should be (setf aref)
(define-lambda-list - (x &rest args))
(define-lambda-list / (x &rest args))
(define-lambda-list slot-value (x &optional y))

(define-lambda-list setq (var val &rest more))
(define-lambda-list lambda (args &rest body))
(define-lambda-list let (bindings &rest body))
(define-lambda-list let* (bindings &rest body))
(define-lambda-list flet (bindings &rest body))
(define-lambda-list labels (bindings &rest body))
(define-lambda-list macrolet (bindings &rest body))
(define-lambda-list if (test cons &optional alt))
(define-lambda-list progn (&rest body))
(define-lambda-list locally (&rest body))
(define-lambda-list the (a b))
(define-lambda-list block (tag &rest body))
(define-lambda-list function (fun))
(define-lambda-list catch (tag &rest forms))
(define-lambda-list throw (tag form))
(define-lambda-list errset (form &optional print))
(define-lambda-list unwind-protect (pform &rest cforms))
(define-lambda-list tagbody (&rest body))
(define-lambda-list go (tag))
(define-lambda-list return-from (tag &optional form))
(define-lambda-list progv (syms vals &rest body))
(define-lambda-list case (key &rest forms))
(define-lambda-list multiple-value-call (funcion &rest forms))
(define-lambda-list nth-value (n form))
(define-lambda-list multiple-value-prog1 (form &rest forms))


(defun check-arg-count (e)
  (let* ((f (first e))
	 (n (length (rest e)))
	 (ll (find-lambda-list-data f)))
    (if ll
	(let ((nr (nth 0 ll))
	      (no (nth 1 ll))
	      (allow-keys (nth 3 ll))
	      (rest (nth 5 ll)))
	  (if (< n nr) (error "too few arguments to ~s" f))
	  (unless (or rest allow-keys)
		  (if (> n (+ nr no))
		      (error "too many arguments to ~s" f)))))))

;;**** be more careful about non-self-evaluating forms
(defun constant-expression-p (e)
  (cond
   ((atom e) (or (not (symbolp e)) (constant-symbol-p e)))
   ((eq (first e) 'quote))))

(defun strip-quote (x) (if (consp x) (second x) x))

;;**** fix these to allow inlining lambdas with rest arguments
;;**** if rest is unused, just drop.
;;**** otherwise, add code to make the list
#|
(defun rest-arg-only-p (n)
  (let* ((ll (lambda-node-lambda-list n))
	 (r (nth 5 ll)))
    (and r (every #'null (remove r (rest (rest (rest ll))))))))
|#

(defun simple-lambda-node-p (n)
  (multiple-value-bind (nr no od ak kd rest aok)
		       (values-list (lambda-node-lambda-list n))
    (and (null ak)
	 (null aok)
	 (or (null rest)
	     (not (any-references-p (first (last (lambda-node-arglist n)))
				    (lambda-node-body n)))))))

(defun fixup-lambda-call-node (n)
  (let ((f (call-node-function n)))
    (if (lambda-node-p f)
	(let* ((ll (lambda-node-lambda-list f))
	       (na (call-node-arg-count n))
	       (nr (first ll))
	       (no (second ll))
	       (rest (sixth ll)))
	  (unless (simple-lambda-node-p n)
		  (error "can't inline a non-simple lambda"))
	  (if (and (not rest) (> na (+ nr no))) (error "too many arguments"))
	  (if (< na nr) (error "too few arguments"))
	  (if (< na (+ nr no))
	      (let ((extra (nthcdr (- na nr) (third ll))))
		(set-node-children n
				   (append
				    (node-children n)
				    (mapcar #'(lambda (x)
						(get-constant-node `',x))
					    extra)))))
	  (if (< (+ nr no) na)
	      (set-node-children n (butlast (node-children n) (- na nr no))))
	  (if rest
	      (lambda-node-delete-arg
	       f
	       (first (last (lambda-node-arglist f)))))))))

(defun lambda-list-num-variables (ll)
  (let ((nr (nth 0 ll))
	(no (nth 1 ll))
	(nk (length (nth 4 ll)))
	(rest (if (nth 5 ll) 1 0))
	(env (if (nth 8 ll) 1 0)))
    (+ nr no nk rest env)))

;;**** fix to use special declarations
(defun fix-lambda-specials (rl)
  (let ((vars (first rl))
	(db (split-declarations (second rl))))
    (when (some #'is-special-variable vars)
	  (let ((bds nil)
		(nv nil)
		(sv nil))
	    (dolist (v vars)
	      (if (is-special-variable v)
		  (let ((s (gensym "SV")))
		    (when (constantp v)
			  (error "can't bind to a constant - ~s" v))
		    (push v bds)
		    (push s bds)
		    (push s nv)
		    (push v sv))
		  (push v nv)))
	    (setf (first rl) (nreverse nv))
	    (setf (second rl)
		  `(,@(first db)
		      (%dynamic-bind ',sv 't
				     #'(lambda ()
					 (setq ,@(nreverse bds))
					 ,@(second db)))))))
    rl))

(defun convert-lambda (e)
  (with-saved-cmp-environments
    (let* ((ksym (gensym "K"))
	   (lambda-list (cons ksym (second e)))
	   (body (rest (rest e)))
	   (rl (fix-lambda-specials (rewrite-lambda lambda-list body)))
	   (vars (mapcar #'new-env-node (first rl)))
	   (k (first vars))
	   (db (split-declarations (second rl)))
	   (new-body `(progn ,@(second db))))
      (make-lambda-node vars (convert new-body k) (rest (rest rl))))))

(define-cps-converter lambda (e k)
  (make-continuation-call-node k (convert-lambda e)))

(define-cps-converter let (e k)
  (let ((bindings (second e))
	(body (rest (rest e))))
    (let ((vars (mapcar #'(lambda (x) (if (consp x) (first x) x)) bindings))
	  (vals (mapcar #'(lambda (x) (if (consp x) (second x))) bindings)))
      (convert `((lambda ,vars ,@body) ,@vals) k))))

(define-cps-converter if (e k)
  (let ((test (second e))
	(consequent (third e))
	(alternative (fourth e)))
    (convert test
	     (with-saved-cmp-environments
	       (let ((v (new-env-node (gensym "V"))))
		 (make-continuation-node
		  (list v)
		  (make-call-node
		   (with-saved-cmp-environments
		     (let ((j (new-env-node (gensym "J"))))
		       (make-lambda-node
			(list j)
			(make-call-node
			 (get-gfun-node '%test)
			 (make-continuation-node () (convert consequent j))
			 (make-continuation-node () (convert alternative j))
			 v))))
		   k)))))))

#|
;****//// this causes deep recursions
(define-cps-converter progn (e k)
  (let ((first (second e))
	(rest (rest (rest e))))
    (if rest
	(convert first
		 (with-saved-cmp-environments
		  (make-continuation-node
		   (list (new-env-node (gensym "V")))
		   (convert `(progn ,@rest) k))))
        (convert first k))))
|#
;;**** iterative version
(define-cps-converter progn (e k)
  (let* ((rbody (reverse (rest e)))
	 (last (first rbody))
	 (rest (rest rbody))
	 (ce (convert last k)))
    (dolist (ee rest ce)
      (setf ce
	    (convert ee
		     (with-saved-cmp-environments
		      (make-continuation-node
		       (list (new-env-node (gensym "V")))
		       ce)))))))

(define-cps-converter locally (e k)
  (convert (cons 'progn (rest e)) k))

(define-cps-converter the (e k)
  (convert (third e) k))

(defun rewrite-let* (e)
  (let* ((v (fixup-let-variables (second e)))
	 (db (split-declarations (rest (rest e))))
	 (body (second db))
	 (d (parse-declarations (first db)))
	 (vd (first d))
	 (fd (second d))
	 (cd (third d)))
    (labels ((rw (v d)
	       (if (consp (rest v))
		   (let* ((vi (first v))
			  (vsym (first vi))
			  (vd (assoc vsym d)))
		     `(let (,vi)
			,@(unparse-variable-declarations vd)
			,(rw (rest v) (remove vd d))))
		   `(let ,v
		      ,@(apply #'append
			       (mapcar #'unparse-variable-declarations d))
		      ,@(unparse-function-declarations fd)
		      ,@(unparse-compiler-declarations cd)
		      ,@body))))
      (rw v vd))))

(define-cps-converter let* (e k) (convert (rewrite-let* e) k))

(defun convert-named-lambda (f)
  (let* ((name (first f))
	 (args (second f))
	 (d (split-declarations (rest (rest f))))
	 (decls (first d))
	 (body (second d)))
    (convert-lambda `(lambda ,args ,@decls (block ,name ,@body)))))

;;**** should use named lambda's!!
;;**** deal with declarations
(define-cps-converter flet (e k)
  (let* ((funs (second e))
	 (db (split-declarations (rest (rest e))))
	 (body `(progn ,@(second db))))
    (flet ((cvtfun (f) (convert-named-lambda f))
	   (fname (f) (new-fenv-node (first f))))
      (let ((cfuns (mapcar #'cvtfun funs)))
	(with-saved-cmp-environments
	 (let* ((alist (mapcar #'fname funs))
		(cbody (make-lambda-node alist (convert body k))))
	   (apply #'make-call-node cbody cfuns)))))))

;;**** should use named lambda's!!
;;**** deal with declarations
(define-cps-converter labels (e k)
  (let* ((funs (second e))
	 (db (split-declarations (rest (rest e))))
	 (body `(progn ,@(second db))))
    (flet ((cvtfun (f) (convert-named-lambda f))
	   (fname (f) (new-fenv-node (first f))))
      (with-saved-cmp-environments
       (let* ((alist (mapcar #'fname funs))
	      (cfuns (mapcar #'cvtfun funs))
	      (cbody (convert body k)))
	 (make-call-node
	  (get-gfun-node '%y)
	  (make-continuation-node
	   alist
	   (apply #'make-call-node 
		  (get-gfun-node '%y-list)
		  (make-continuation-node nil cbody)
		  cfuns))))))))

(define-cps-converter block (e k)
  (let ((name (second e))
	(body (rest (rest e))))
    (with-saved-cmp-environments
     (let ((tag (new-env-node (gensym "T")))
	   (kk (new-env-node (gensym "K"))))
       (push (list 'block name tag) *cmp-tenv*)
       (make-call-node
	(get-gfun-node '%catch-block)
	  k
	  (get-constant-node `(quote ,name))
	  (make-lambda-node (list kk tag) (convert `(progn ,@body) kk)))))))

(defun find-block-tag (name)
  (dolist (a *cmp-tenv*)
    (if (and (eq 'block (first a)) (eq name (second a)))
	(return (first (rest (rest a)))))))
		
;;**** simplify this?
(define-cps-converter setq (e k)
  (let ((sym (second e))
	(val (third e))
	(rest (rest (rest (rest e)))))
    (if rest
	(convert `(progn (setq ,sym ,val) (setq ,@rest)) k)
        (convert val
		 (with-saved-cmp-environments
		  (let ((v (new-env-node (gensym "V")))
			(sn (get-variable-node sym)))
		    (if (gvar-node-p sn)
			(progn
			  (when (constantp sym)
				(error
				 "can't change the value of a constant - ~s"
				 sym))
			  (make-continuation-node
			   (list v)
			   (make-call-node
			    (get-gfun-node '%set-symval)
			    k
			    (get-constant-node `(quote ,(leaf-node-value sn)))
			    v)))
		        (make-continuation-node
			 (list v)
			 (make-call-node (get-gfun-node '%setq) k sn v)))))))))

(define-cps-converter function (e k)
  (let ((f (second e)))
    (cond
     ((symbolp f)
      (let ((fn (get-function-node f)))
	(if (gfun-node-p fn)
	    (make-call-node (get-gfun-node '%symfun) k fn)
	    (make-continuation-call-node k fn))))
     ((and (consp f) (eq (first f) 'lambda)) (convert f k))
     (t (error "bad argument for FUNCTION -- ~s" f)))))

(define-cps-converter catch (e k)
  (convert `(%catch ,(second e) (lambda () ,@(rest (rest e)))) k))

(define-cps-converter errset (e k)
  (let ((expr (second e))
	(flag (if (= (length e) 3) (third e) t)))
    (convert `(%errset (lambda () ,expr) ,flag) k)))

(define-cps-converter unwind-protect (e k)
  (convert `(%unwind-protect #'(lambda () ,(second e))
			     #'(lambda () ,@(rest (rest e))))
	   k))

(define-cps-converter progv (e k)
  (let ((vars (second e))
	(vals (third e))
	(body (rest (rest (rest e)))))
    (convert `(%progv ,vars ,vals #'(lambda () ,@body)) k)))

;;**** is this the best way?
(define-cps-converter case (e k)
  (let ((var (second e))
	(sels (mapcar #'first (rest (rest e))))
	(acts (mapcar #'rest (rest (rest e)))))
    (if (member (first (last sels)) '(t otherwise))
	(setf sels (butlast sels))
        (setf acts (append acts (list nil))))
    (convert var
	     (with-saved-cmp-environments
	      (let ((v (new-env-node (gensym "V"))))
		(make-continuation-node
		 (list v)
		 (make-call-node
		  (with-saved-cmp-environments
		   (let ((j (new-env-node (gensym "K"))))
		    (flet ((mkfun (a)
			     (make-continuation-node
			      ()
			      (convert `(progn ,@a) j))))
		      (make-continuation-node
		       (list j)
		       (apply #'make-call-node
			      (get-gfun-node '%case)
			      v
			      (get-constant-node
			       `(quote ,(coerce sels 'vector)))
			      (mapcar #'mkfun acts))))))
		 k)))))))

(defun split-tagbody-body (bd)
  (unless (symbolp (first bd)) (push (gensym "S") bd))
  (let ((pairs nil)
	(tag (first bd))
	(body nil))
    (dolist (f (rest bd))
      (cond
       ((consp f) (push f body))
       (t (push (list tag (reverse body)) pairs)
	  (setf tag f)
	  (setf body nil))))
    (push (list tag (reverse body)) pairs)
    (setf pairs (reverse pairs))
    (list (mapcar #'first pairs) (mapcar #'second pairs))))

(define-cps-converter tagbody (e k)
  (let ((bd (rest e)))
    (cond
     ((null bd) (convert nil k))
     ((every #'consp bd) (convert `(progn ,@bd nil) k))
     (t (let* ((sb (split-tagbody-body bd))
	       (gtags (first sb))
	       (gbd (reverse (second sb))))
	  (with-saved-cmp-environments
	   (let ((gnodes (mapcar #'(lambda (x) (new-fenv-node (gensym x)))
				 gtags))
		 (fnodes nil)
		 (kk (new-env-node (gensym "K")))
		 (tag (new-env-node (gensym "T"))))
	     (with-saved-cmp-environments
	      (push (list 'tagbody gtags gnodes tag) *cmp-tenv*)
	      (push (make-continuation-node
		     nil
		     (convert `(progn ,@(first gbd))
			      (make-continuation-node
			       (list (new-env-node (gensym "V")))
			       (make-continuation-call-node
				kk
				(get-constant-node nil)))))
		    fnodes))
	     (mapc #'(lambda (x y)
		       (with-saved-cmp-environments
			(push (list 'tagbody gtags gnodes tag) *cmp-tenv*)
			(push (make-continuation-node
			       nil
			       (convert `(progn ,@x)
					(make-continuation-node
					 (list (new-env-node (gensym "V")))
					 (make-call-node y))))
			      fnodes)))
		   (rest gbd)
		   (reverse (rest gnodes)))
	     (make-call-node
	      (get-gfun-node '%catch-tagbody)
	      k
	      (make-lambda-node
	       (list kk tag)
	       (make-call-node
		(get-gfun-node '%y)
		(make-continuation-node
		 gnodes
		 (apply #'make-call-node
			(get-gfun-node '%y-list)
			(make-continuation-node
			 nil
			 (make-call-node
			  (get-gfun-node '%do-catch-tagbody)
			  k
			  (first gnodes)
			  kk
			  tag))
			fnodes))))))))))))

(defun find-go-tag (tag)
  (dolist (a *cmp-tenv*)
    (if (and (eq 'tagbody (first a)) (member tag (second a)))
	(return (list (fourth a) (nth (position tag (second a)) (third a)))))))
		
(define-cps-converter go (e k)
  (let* ((tag-name (second e))
	 (tag-info (find-go-tag tag-name))
	 (tag (first tag-info))
	 (fun (second tag-info)))
    (if tag
	(make-call-node (get-gfun-node '%throw-go) tag fun)
        (error "no tag named ~s in current lexical context" tag-name))))

;;**** ought to push multiple values on stack to avoid consing for > 1 expr
(define-cps-converter multiple-value-call (e k)
  (case (length e)
    (2 (convert `(funcall ,(second e)) k))
    (3
     (let ((fun (second e))
	   (expr (third e)))
       (convert fun
		(with-saved-cmp-environments
		 (let ((v (new-env-node (gensym "F")))
		       (w (new-env-node (gensym "V"))))
		   (make-continuation-node
		    (list v)
		    (convert expr
			     (make-continuation-node
			      (list w)
			      (make-call-node
			       (get-gfun-node '%mv-collect)
			       (make-continuation-node
				(list (new-env-node (gensym "D")))
				(make-call-node (get-gfun-node '%mvc)
						k
						v))
			       w)))))))))
    (t
     (convert `(apply ,(second e)
		      (nconc ,@(mapcar #'(lambda (x) `(multiple-value-list ,x))
				       (rest (rest e)))))
	      k))))


;;****** uses multiple-value-continuation
(define-cps-converter nth-value (e k)
  (let ((count (second e))
	(expr (third e)))
    (convert count
	     (with-saved-cmp-environments
	      (let ((v (new-env-node (gensym "C")))
		    (w (new-env-node (gensym "V"))))
		(make-continuation-node
		 (list v)
		 (convert expr
			  (make-continuation-node
			   (list w)
			   (make-call-node
			    (get-gfun-node '%mv-collect)
			    (make-continuation-node
			     (list (new-env-node (gensym "D")))
			     (make-call-node (get-gfun-node '%nth-value)
					     k
					     v))
			    w)))))))))

(define-cps-converter throw (e k)
  (let ((tag (second e))
	(expr (third e)))
    (convert tag
	     (with-saved-cmp-environments
	      (let ((v (new-env-node (gensym "C")))
		    (w (new-env-node (gensym "V"))))
		(make-continuation-node
		 (list v)
		 (convert expr
			  (make-continuation-node
			   (list w)
			   (make-call-node
			    (get-gfun-node '%mv-collect)
			    (make-continuation-node
			     (list (new-env-node (gensym "D")))
			     (make-call-node (get-gfun-node '%throw) v))
			    w)))))))))

(define-cps-converter return-from (e k)
  (let* ((name (second e))
	 (expr (third e))
	 (tag (find-block-tag name)))
    (if tag
	(with-saved-cmp-environments
	 (let ((w (new-env-node (gensym "V"))))
	   (convert expr
		    (make-continuation-node
		     (list w)
		     (make-call-node
		      (get-gfun-node '%mv-collect)
		      (make-continuation-node
		       (list (new-env-node (gensym "D")))
		       (make-call-node (get-gfun-node '%throw-return-from)
				       tag))
		      w)))))
        (error "no block named ~s in current lexical context" name))))

(define-cps-converter multiple-value-prog1 (e k)
  (let* ((expr (second e))
	 (body (rest (rest e))))
    (with-saved-cmp-environments
     (let ((w (new-env-node (gensym "V")))
	   (n (new-env-node (gensym "N"))))
       (convert expr
		(make-continuation-node
		 (list w)
		 (make-call-node
		  (get-gfun-node '%mv-collect)
		  (make-continuation-node
		   (list (new-env-node (gensym "D")))
		   (make-call-node
		    (get-gfun-node '%push-values)
		    (make-continuation-node
		     (list n)
		     (convert `(progn ,@body)
			      (make-continuation-node
			       (list (new-env-node (gensym "D")))
			       (make-call-node (get-gfun-node '%pop-values)
					       k
					       n))))))
		  w)))))))

;;***** this could use some decent error checking
(define-cps-converter macrolet (e k)
  (let ((macs (second e))
	(env (list nil *cmp-fenv* *cmp-macros* *cmp-global-macros*))
	(frame nil)
	(body (rest (rest e))))
    (with-saved-cmp-environments
     (dolist (m macs)
       (push (cons (first m)
		   (coerce-to-macro
		    (parse-macro (first m) (second m) (rest (rest m)) env)))
	     frame))
     (dolist (x frame) (push x *cmp-fenv*))
     (convert `(progn ,@body) k))))

(defun substitute-all-variables (n)
  (flet ((substitute1 (n)
	   (if (call-node-p n)
	       (let ((f (call-node-function n)))
		 (when (lambda-node-p f)
		       (lambda-call-substitute-variables n))))))
    (do () ((not (substitute1 n))))
    (dolist (c (node-children n)) (substitute-all-variables c))))

(defun collapse-null-lambda-calls (n)
  (flet ((collapse1 (n)
	   (if (call-node-p n)
	       (let ((f (call-node-function n)))
		 (when (and (lambda-node-p f) (null (lambda-node-arglist f)))
		       (move-node-tree (lambda-node-body f) n)
		       t)))))
    (do () ((not (collapse1 n))))
    (dolist (c (node-children n)) (collapse-null-lambda-calls c))))
