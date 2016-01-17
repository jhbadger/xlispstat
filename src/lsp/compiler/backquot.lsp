;;; Backquote Implementation from Common Lisp
;;; Author: Guy L. Steele Jr.  Date: 27 December 1985
;;; This software is in the public domain


;;; TAA notes:
;;; Converted to XLISP from the CLtL book, July, 1991, by Tom Almy
;;; Expression simplification code removed.

;;; Reader Macros -- already exist for ` , and ,@ that generate correct
;;;  code for this backquote implementation.

;;; This implementation will execute far slower than the XLISP original, 
;;; but since macros expansions can replace the original code
;;; (at least with my modified XLISP implementation)
;;; most applications will run at their full speed after the macros have
;;; been expanded once.

(in-package "XLISP")

(defun bq-process (x)
       (cond ((atom x) (list 'quote x))
	     ((eq (car x) 'backquote)
	      (bq-process (bq-process (cadr x))))
	     ((eq (car x) 'comma) (cadr x))
	     ((eq (car x) 'comma-at)
	      (error ",@ after ` in ~s" (cadr x)))
	     (t (do ((p x (cdr p))
		     (q '() (cons (bq-bracket (car p)) q)))
		    ((atom p)
		     (if (null p)	;; simplify if proper list TAA MOD
			 (cons 'append (nreverse q))
			 (cons 'append
			       (nconc (nreverse q) (list (list 'quote p))))))
		    (when (eq (car p) 'comma)
			  (unless (null (cddr p)) (error "Malformed: ~s" p))
			  (return (cons 'append
					(nconc (nreverse q) 
					       (list (cadr p))))))
		    (when (eq (car p) 'comma-at)
			  (error "Dotted ,@ in ~s" p))
		    ))))

(defun bq-bracket (x)
       (cond ((atom x)
	      (list 'list (list 'quote x)))
	     ((eq (car x) 'comma)
	      (list 'list (cadr x)))
	     ((eq (car x) 'comma-at)
	      (cadr x))
	     (t (list 'list (bq-process x)))))

(defmacro backquote (x)
	  (bq-process x))

(setq *features* (cons :backquote *features*))
