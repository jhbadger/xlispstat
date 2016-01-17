;;***** fix handling of throws and return-froms

(in-package "XLSCMP")

;;;;;
;;;;; Lambda Lifting
;;;;;

(defvar *pieces*)

;;***** is this right??
(defun free-variable-candidates (n)
  (if (gfun-eq (call-node-function n) '%throw-go)
      (list (call-node-arg n 0))
      (call-node-args n)))			    

(defun find-free-variables (n)
  (let ((vars (lambda-node-arglist n))
	(free nil)
	(nodes (list n)))
    (flet ((is-free-variable (v)
	     (if (local-symbol-node-p v)
		 (do ((m (symbol-node-owner v) (node-parent m)))
		     ((null m) t)
		     (if (eq m n) (return nil))))))
      (loop
       (if (null nodes) (return))
       (let ((n (pop nodes)))
	 (if (call-node-p n)
	     (dolist (a (free-variable-candidates n))
	       (if (is-free-variable a) (push a free))))
	 (dolist (c (node-children n)) (push c nodes))))
      (remove-duplicates free))))

(defun find-all-free-variables (nl)
  (let ((free nil))
    (dolist (n nl)
      (let ((fv (find-free-variables n)))
	(dolist (v fv) (pushnew v free))))
    free))

(defun find-and-fix-liftable-lambdas (n)
  (let ((flet-lambdas nil)
	(labels-lambdas nil)
	(nodes (list n)))
    (flet ((is-liftable (n)
	     (and (lambda-node-p n) (not (continuation-node-p n)))))
      (loop
       (if (null nodes) (return))
       (let ((n (pop nodes)))
	 (if (call-node-p n)
	     (let ((f (call-node-function n)))
	       (cond
		((lambda-node-p f)
		 (let ((names (lambda-node-arglist f))
		       (args (call-node-args n))
		       (body (lambda-node-body f)))
		   (mapcar #'(lambda (x y)
			       (when (is-liftable y)
				     (fix-liftable-lambda x y body)
				     (push (list x y) flet-lambdas)))
			   names
			   args)))
		(t
		 (case (gfun-symbol f)
		       (%y-list
			(let ((names nil)
			      (args nil)
			      (body (call-node-arg n 0)))
			  (mapc #'(lambda (x y)
				    (when (is-liftable y)
					  (push x names)
					  (push y args)
					  (push (list x y) labels-lambdas)))
				(find-y-list-names n)
				(rest (call-node-args n)))
			  (fix-liftable-y-lambdas n names args body))))))))
	 (dolist (c (node-children n)) (push c nodes))))
      (list flet-lambdas labels-lambdas))))

;;**** handle closures
;;**** handle %y, etc.
(defun new-lvar-node (v) (make-variable-node (leaf-node-value v)))
(defun new-lfun-node (v) (make-variable-node (leaf-node-value v)))

(defun substitute-variable (nv v f)
  (dolist (r (find-references v f)) (substitute-value nv r nil)))

(defun fix-liftable-lambda-arglist (f free)
  (let* ((nvars (mapcar #'new-lvar-node free))
	 (alist (lambda-node-arglist f))
	 (new-alist (cons (first alist) (append nvars (rest alist)))))
    (set-lambda-node-arglist f new-alist)
    ;;**** should be somewhere else??
    (dolist (v nvars) (set-symbol-node-owner v f))
    ;;**** a hack
    (let* ((fd (lambda-node-lambda-list f))
	   (nr (first fd)))
      (setf (first fd) (+ nr (length nvars))))
    (mapc #'(lambda (v nv) (substitute-variable nv v f)) free nvars)))

(defun fix-liftable-lambda-calls (vf body free)
  (dolist (r (find-references vf body))
    (let ((form (car r))
	  (pos (cdr r)))
      (if (= pos 0)
	  (let* ((args (call-node-args form))
		 (new-args (cons (first args) (append free (rest args)))))
	    (set-call-node-args form new-args))
          (let ((f (call-node-function form)))
	    (if (gfun-eq f '%make-closure)
		(set-call-node-args form
				    (append (call-node-args form) free))))))))

(defun fix-liftable-lambda (vf f body)
  (let ((free (find-free-variables f)))
    (when free
	  (fix-liftable-lambda-calls vf body free)
	  (fix-liftable-lambda-arglist f free))))

(defun fix-liftable-y-lambdas (n vfl fl body)
  (let ((free (find-all-free-variables fl)))
    (when free
	  (dolist (vf vfl)
	    (dolist (f (cons body fl))
	      (fix-liftable-lambda-calls vf f free)))
	  (mapc #'(lambda (vf f) (fix-liftable-lambda-arglist f free))
		vfl
		fl)
	  (let* ((pp (node-parent (node-parent n)))
		 (pf (call-node-function pp)))
	    (if (gfun-eq pf '%make-y-closures)
		(set-call-node-args pp (append (call-node-args pp) free)))))))

(defun lift-fixed-lambdas (nl)
  (dolist (l nl)
    (let* ((v (first l))
	   (f (second l))
	   (p (node-parent f))
	   (g (symbol-node-owner v)))
      (call-node-delete-arg p f) ;;**** fix
      (lambda-node-delete-arg g v)
      (set-symbol-node-owner v nil)
      (push (list v f) *pieces*))))

;;**** safe lambda args -- make into continuations??
(defun hoist-anonymous-closures (n)
  (let ((nodes (list n)))
    (loop
     (if (null nodes) (return))
     (let ((n (pop nodes)))
       (if (call-node-p n)
	   (let ((f (call-node-function n)))
	     (unless (or (lambda-node-p f)
			 (gfun-member f ;;**** check these out
				      '(%y-list
					%catch %throw
					%catch-block %throw-return-from
					%catch-tagbody %throw-go
					%errset
					%unwind-protect
					%dynamic-bind)))
		     (let ((fvars nil)
			   (funs nil)
			   (args (call-node-args n)))
		       (dolist (a args);**** is this right???
		         (if (and (lambda-node-p a)
				  (not (continuation-node-p a)))
			     (let ((v (make-leaf-node (gensym "F"))))
			       (push v fvars)
			       (push a funs)
			       (set-call-node-arg n (position a args) v))))
		       (if funs
			   (let ((temp (make-node)))
			     (move-node-tree n temp)
			     (move-node-tree
			      (apply #'make-call-node
				     (make-lambda-node fvars temp)
				     funs)
			      n)))))))
       (dolist (c (node-children n)) (push c nodes))))))

(defun find-closure-references (v body)
  (let ((crefs nil))
    (dolist (r (find-references v body) crefs)
      (if (/= (cdr r) 0) (push r crefs)))))

(defun insert-flet-closures (n)
  (let* ((f (call-node-function n))
	 (cvars nil)
	 (crefs nil)
	 (args (call-node-args n))
	 (alist (lambda-node-arglist f))
	 (body (lambda-node-body f)))
    (dolist (a args)
      (if (and (lambda-node-p a) (not (continuation-node-p a)))
	  (let* ((v (nth (position a args) alist))
		 (cr (find-closure-references v body)))
	    (when cr
		  (push v cvars)
		  (push cr crefs)))))
    (if crefs
	(let ((ncvars (mapcar #'new-lfun-node cvars)))
	  (mapc #'(lambda (nvf cr)
		    (dolist (r cr)
		      (set-call-node-arg (car r) (- (cdr r) 1) nvf)))
		ncvars
		crefs)
	  (mapc #'(lambda (vf nvf)
		    (let ((temp (make-node))
			  (b (lambda-node-body f)))
		      (move-node-tree b temp)
		      (move-node-tree
		       (make-call-node (get-gfun-node '%make-closure)
				       (make-continuation-node (list nvf) temp)
				       vf)
		       b)))
		cvars
		ncvars)))))

(defun insert-labels-closures (n)
  (let ((fvars (find-y-list-names n))
	(cvars nil)
	(crefs nil))
    (dolist (f fvars)
      (if (not (continuation-node-p (find-lambda-binding f)))
	  (let ((cr (find-closure-references f n)))
	    (when cr
		  (push f cvars)
		  (push cr crefs)))))
    (if cvars
	(let ((temp (make-node))
	      (ncvars (mapcar #'new-lfun-node cvars)))
	  (mapc #'(lambda (nvf cr)
		    (dolist (r cr)
		      (set-call-node-arg (car r) (- (cdr r) 1) nvf)))
		ncvars
		crefs)
	  (move-node-tree n temp)
	  (move-node-tree (apply #'make-call-node
				 (get-gfun-node '%make-y-closures)
				 (make-continuation-node ncvars temp)
				 cvars)
			  n)))))

(defun insert-closures (n)
  (if (call-node-p n)
      (let ((f (call-node-function n)))
	(cond
	 ((lambda-node-p f) (insert-flet-closures n))
	 ((gfun-eq f '%y-list) (insert-labels-closures n)))))
  (dolist (c (node-children n)) (insert-closures c)))

(defun lift-lambdas (n)
  (hoist-anonymous-closures n)
  (insert-closures n)
  (let ((lambdas (find-and-fix-liftable-lambdas n)))
    (progv '(*pieces*) (list (list (list (make-leaf-node (gensym "MAIN")) n)))
      (lift-fixed-lambdas (first lambdas))
      (lift-fixed-lambdas (second lambdas))
      (reverse *pieces*))))


;;;;
;;;; Extract Constants
;;;;

(defun extract-constants (n)
  (let ((consts nil)
	(nodes (list n)))
    (flet ((symfunp (f) (gfun-member f '(%symval %set-symval %symfun)))
	   (get-constant-variable (c)
	     (let ((ve (assoc c consts)))
		(if ve
		    (second ve)
	            (let ((v (make-leaf-node (gensym "C"))))
		      (push (list c v) consts)
		      v)))))
      (loop
       (if (null nodes) (return))
       (let ((n (pop nodes)))
	 (let ((ch (node-children n)))
	   (dolist (c (node-children n))
	     (if (and (leaf-node-p c)
		      (not (and (symfunp (call-node-function n))
				(= 2 (position c ch)))))
		 (if (constant-node-p c)
		     (setf (nth (position c ch) ch)
			   (get-constant-variable c)))
	       (push c nodes))))))
      (if consts
	  (let ((b (make-node)))
	    (move-node-tree (lambda-node-body n) b)
	    (move-node-tree (apply #'make-call-node
				   (make-lambda-node
				    (mapcar #'second consts)
				    b)
				   (mapcar #'first consts))
			    (lambda-node-body n)))))))
