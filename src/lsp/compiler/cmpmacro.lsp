(in-package "XLSCMP")

;;;;
;;;; Compiler Macro Expansion
;;;;

;;**** move to proper place?
(defvar *cmp-macros* nil)
(defvar *cmp-global-macros* nil)

;;**** think about precedence if macro and cmpmacro both exist
;;**** may simplify setf that way?

(defun cmp-macroexpand (e &optional (env (list nil
					       *cmp-fenv*
					       *cmp-macros*
					       *cmp-global-macros*)))
  (macroexpand e env))

(defun cmp-macroexpand-1 (e &optional (env (list nil
						 *cmp-fenv*
						 *cmp-macros*
						 *cmp-global-macros*)))
  (macroexpand-1 e env))


;;;;
;;;; Declaration Handling
;;;;

(defun check-declarations (decls)
  (dolist (d decls)
    (dolist (i (rest d))
      (if (and (consp i) (eq (first i) 'special))
	  (dolist (v (rest i))
	    (warn "special declaration for ~s ignored." v))))))

(defun split-declarations (x)
  (flet ((head-is-declaration (x)
	   (and (consp (first x)) (eq (first (first x)) 'declare)))
	 (head-is-docstring (x) (and (stringp (first x)) (consp (rest x)))))
    (do ((decls nil)
	 (body x (rest body))
	 (doc nil))
	(nil)
	(cond
	 ((head-is-declaration body) (push (first body) decls))
	 ((head-is-docstring body) (setf doc (first body)))
	 (t (check-declarations decls)
	    #|(return (list (nreverse decls) body doc))|#
	    (return (list nil body doc))))))) ; drop declarations for now


;;;;
;;;; PROGV
;;;;

(define-compiler-macro progv (syms vals &rest body)
  `(%dynamic-bind ,syms ,vals #'(lambda () ,@body)))



;;;;
;;;; Macros for inlining some functions
;;;; ******* more needed here -- should these be here or as symbol-call-rules??

(define-compiler-macro not (x) `(if ,x nil t))
(define-compiler-macro null (x) `(if ,x nil t))

(define-compiler-macro row-major-aref (x i) `(aref ,x ,i))
(define-compiler-macro xlisp::%set-rm-aref (x i v)
  `(xlisp::%set-aref ,x ,i ,v))
