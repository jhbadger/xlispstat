(in-package "XLSCMP")

;;;;;
;;;;; Cell Insertion Phase
;;;;;

;;**** check this over carefully!!!
;;**** assumes cells can be determined from tree
;;**** change %setq to %set-cell-value here
(defun find-variables-set (tree)
  (let ((svars nil)
	(nodes (list tree)))
    (loop
     (if (null nodes) (return))
     (let ((n (pop nodes)))
       (dolist (c (node-children n))
	 (if (call-node-p c)
	     (let ((f (call-node-function c)))
	       (if (gfun-eq f '%setq)
		   (push (call-node-arg c 1) svars))))
	 (push c nodes))))
    svars))

(defun insert-cells (tree)
  (let ((svars (find-variables-set tree)))
    (dolist (sv (remove-duplicates svars))
      (let* ((owner (symbol-node-owner sv))
	     (ob (lambda-node-body owner))
	     (refs (find-references sv ob))
	     (cn (make-leaf-node (gensym "C"))))
	(insert-node-tree
	 (make-call-node (get-gfun-node '%make-cell)
			 (make-continuation-node (list cn) ob)
			 sv)
	 owner
	 0)
	(dolist (r refs)
          (let* ((form (car r))
		 (i (cdr r))
		 (f (call-node-function form))
		 (p (node-parent form))
		 (j (position form (node-children p))))
	    (if (and (gfun-eq f '%setq) (= i 2))
		(substitute-value cn r nil) ;**** better choice??
	        (let ((vn (make-leaf-node (gensym "V"))))
		  (substitute-value vn r nil) ;**** better choice??
		  (insert-node-tree
		   (make-call-node (get-gfun-node '%cell-value)
				   (make-continuation-node
				    (list vn)
				    form)
				   cn)
		   p
		   j))))))))
  (flet ((fixup (s ns n)
	   (let ((nf (get-gfun-node ns)))
	     (dolist (r (find-references (get-gfun-node s) n))
	       (substitute-value nf r nil)))))
    (fixup '%setq '%set-cell-value tree))
  tree)

;;;;;
;;;;; Drop Unused Cells
;;;;;
				     
(defun cell-used-p (refs n)
  (dolist (r refs nil)
    (let* ((form (car r))
	   (i (cdr r))
	   (f (call-node-function form)))
      (unless (leaf-node-p f) (return t))
      (case (gfun-symbol f)
        ((%cell-value %set-cell-value)
	 (unless (= i 2) (return t)))
	(t (return t))))))

(defun find-unused-cell-variable-references (n)
  (let ((refs (find-references (get-gfun-node '%make-cell) n))
	(vrlist nil))
    (dolist (r refs vrlist)
      (let* ((c (call-node-arg (car r) 0))
	     (cc (if (lambda-node-p c) c (find-lambda-binding c)))
	     (v (first (lambda-node-arglist cc)))
	     (vrefs (find-references v n)))
	(unless (cell-used-p vrefs n)
		(push (list v (car r) vrefs) vrlist))))))

#|
;; **** this should produce correct results, but it may not get rid of all
;; **** the copies that could be dropped.
(defun safe-cell-access-p (ref)
  (let* ((form (car ref))
	 (var (nth (cdr ref) (node-children form))) ;;****more efficient way?
	 (cc (symbol-node-owner var)))
    (do ((p (node-parent form) (node-parent p)))
	((eq p cc) t)
	(if (and (call-node-p p)
		 (not (gfun-member (call-node-function p)
				   '(%copy %cell-value))))
	    (return nil)))))
|#
;; **** this is a little more agressive, but should still be OK
;; **** it would be better if the function could be recognized as a real fun
(defun safe-cell-access-p (ref)
  (let* ((form (car ref))
	 (var (nth (cdr ref) (node-children form))) ;;****more efficient way?
	 (cc (symbol-node-owner var)))
    (do ((p (node-parent form) (node-parent p)))
	((eq p cc) t)
	(if (call-node-p p)
	    (let ((f (call-node-function p))
		  (a (call-node-args p)))
	      (cond
	       ((lambda-node-p f) (if (some #'lambda-node-p a) (return nil)))
	       ((gfun-member f '(%setq %set-cell-value))
		(let ((cell (call-node-arg (node-parent cc) 1)))
		  (if (eq cell (second a)) (return nil))))
	       (t (if (some #'lambda-node-p (rest a)) (return nil)))))))))

(defun remove-unused-cells (n)
  (let ((cvars (find-unused-cell-variable-references n)))
    (dolist (vr cvars)
      (let* ((cname (first vr))
	     (refs (third vr))
	     (mcf (second vr))
	     (v (call-node-arg mcf 1))
	     (c (call-node-arg mcf 0)))
	(dolist (r refs)
	  (let ((form (car r)))
	    (case (gfun-symbol (call-node-function form))
	      (%set-cell-value
	       (change-gfun form '%setq))
	      (%cell-value
	       (let* ((cc (call-node-arg form 0))
		      (vv (first (lambda-node-arglist cc)))
		      (vvrefs (find-references vv cc)))
		 (cond
		  ((every #'safe-cell-access-p vvrefs)
		   (dolist (r vvrefs) (substitute-value cname r nil))
		   (set-lambda-node-arglist-fix cc nil)
		   (set-node-children form (list cc))
		   (set-node-parent cc form))
		  (t (change-gfun form '%copy))))))))
	(change-gfun mcf '%copy))))
  (collapse-null-lambda-calls n))
