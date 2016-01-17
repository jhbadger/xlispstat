;; Pattern matching Chapter 24 Winston&Horn 3rd Edition


(defun add-binding (pve datum bindings)
	(if (eq '_ (extract-variable pve))
	    bindings
	    (cons (make-binding (extract-variable pve) datum) bindings)))

(defun extract-variable (pve) (second pve))

(defun make-binding (variable datum) (list variable datum))

(defun find-binding (pve binding)
	(unless (eq '_ (extract-variable pve))
		(assoc (extract-variable pve) binding)))


(defun extract-key (binding) (first binding))
(defun extract-value (binding) (second binding))

(defun match-atoms (p d bindings)
	(if  (eql p d)
	     bindings
	     'fail))

(defun match-variable (p d bindings)
	(let ((binding (find-binding p bindings)))
	     (if binding
	         (match (extract-value binding) d bindings)
		 (add-binding p d bindings))))

(defun match-pieces (p d bindings)
	(let ((result (match (first p) (first d) bindings)))
	     (if (eq 'fail result)
	         'fail
		 (match (rest p) (rest d) result))))

(defun elements-p (p d)
	(and (atom p) (atom d)))

(defun variable-p (p)
	(and (listp p) (eq '? (first p))))

(defun recursive-p (p d)
	(and (listp p) (listp d)))

(defun match (p d &optional bindings)
	(cond ((elements-p p d)
	       (match-atoms p d bindings))
	      ((variable-p p)
	       (match-variable p d bindings))
	      ((recursive-p p d)
	       (match-pieces p d bindings))
	      (t 'fail)))

(defun unify-atoms (p1 p2 bindings)
	(if  (eql p1 p2)
	     bindings
	     'fail))

(defun unify-pieces (p1 p2 bindings)
	(let ((result (unify (first p1) (first p2) bindings)))
	     (if (eq 'fail result)
	         'fail
		 (unify (rest p1) (rest p2) result))))

(defun insidep (variable expression bindings)
	(if (equal variable expression)
	    nil
	    (inside-or-equal-p variable expression bindings)))

(defun inside-or-equal-p (variable expression bindings)
   (cond ((equal variable expression) t)
   	 ((atom expression) nil)
	 ((eq '? (first expression))
	  (let ((binding (find-binding expression bindings)))
	       (when binding
	             (inside-or-equal-p variable (first expression) bindings))))
	 (t (or (inside-or-equal-p variable (first expression) bindings)
	 	(inside-or-equal-p variable (rest expression) bindings)))))

(defun unify-variable (p1 p2 bindings)
	(let ((binding (find-binding p1 bindings)))
	     (if binding
	         (unify (extract-value binding) p2 bindings)
		 (if (insidep p1 p2 bindings)
		     'fail
		     (add-binding p1 p2 bindings)))))

(defun unify (p1 p2 &optional bindings)
	(cond ((elements-p p1 p2)
	       (unify-atoms p1 p2 bindings))
	      ((variable-p p1)
	       (unify-variable p1 p2 bindings))
	      ((variable-p p2)
	       (unify-variable p2 p1 bindings))
	      ((recursive-p p1 p2)
	       (unify-pieces p1 p2 bindings))
	      (t 'fail)))










