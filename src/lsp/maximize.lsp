;;;; maxmize.lsp -- maximization and derivative code for XLISP-STAT 
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the
;;;; file COPYING included with this distribution.

(provide "maximize")

(in-package "XLISP")

(export '(numgrad numhess newtonmax nelmeadmax))

;;**** allow typf to be specified?

(defparameter *default-derivative-step* (^ machine-epsilon (/ 1.0 6.0)))

(defmacro make-fundata () `(make-array 7))
(defmacro fundata-f (fd) `(aref ,fd 0))
(defmacro fundata-arg (fd) `(aref ,fd 1))
(defmacro fundata-typx (fd) `(aref ,fd 2))
(defmacro fundata-fsum (fd) `(aref ,fd 3))
(defmacro fundata-n (fd) `(aref ,fd 4))
(defmacro fundata-changesign (fd) `(aref ,fd 5))
(defmacro fundata-h (fd) `(aref ,fd 6))

(defmacro get-function-value (f x arg cs)
  (let ((xsym (gensym "X"))
	(asym (gensym "ARG")))
    `(let ((,xsym ,x)
	   (,asym ,arg))
       (replace ,asym ,xsym)
       (let* ((v (funcall ,f ,asym))
	      (fv (if (consp v) (first v) v)))
	 (if ,cs (- fv) fv)))))

(defun change-array-sign (x)
  (let ((x (compound-data-seq x)))
    (if (typep x '(vector float))
	(blas-dscal (length x) -1.0 x 0 1)
        (dotimes (i (length x)) (setf (aref x i) (- (aref x i)))))))


;;;;
;;;; Functions Evaluation Routines
;;;;

;;;
;;; All Hessian evaluations by numerical derivatives assume the gradient is
;;; evaluated first at the same location. The results are cached away.
;;;

(defun make-function-data (f n changesign h typx)
  (let ((data (make-fundata)))
    (setf (fundata-f data) f)
    (setf (fundata-arg data) (make-list n))
    (setf (fundata-typx data)
	  (make-array n :element-type 'float :initial-element 1.0))
    (setf (fundata-fsum data) (make-array n :element-type 'float))
    (setf (fundata-n data) n)
    (setf (fundata-changesign data) changesign)
    (setf (fundata-h data) (if (and h (> h 0.0)) h *default-derivative-step*))
    (when typx
	  (let ((newtypx (fundata-typx data)))
	    (dotimes (i n)
	      (let ((txi (elt typx i)))
		(setf (aref newtypx i) (if (> txi 0.0) txi 1.0))))))
    data))

(defun eval-function (func x grad hess)
  (let* ((f (fundata-f func))
	 (arg (fundata-arg func))
	 (typx (fundata-typx func))
	 (fsum (fundata-fsum func))
	 (n (fundata-n func))
	 (cs (fundata-changesign func))
	 (h (fundata-h func)))
    (replace arg x)
    (let* ((result (funcall f arg))
	   (val (if (consp result) (car result) result)))
      (when result
	    (let ((fderivs (if (consp result) (- (length result) 1) 0)))
	      (when cs (setf val (- val)))
	      (when grad
		    (cond
		     ((> fderivs 0)
		      (replace grad (car (cdr result)))
		      (when cs (change-array-sign grad)))
		     (t
		      (cmpgrad n x grad fsum func h typx))))
	      (when hess
		    (cond
		     ((= fderivs 2)
		      (replace (compound-data-seq hess)
			       (compound-data-seq (car (cdr (cdr result)))))
		      (when cs (change-array-sign hess)))
		     (t
		      ;;**** fix this for hess from grad?
		      (when (and (= fderivs 1) (null grad))
			    ;; kludge to get fsum for analytic gradients
			    (cmpgrad n x fsum fsum func h typx))
		      (cmphess n x hess val fsum func h typx))))))
      val)))

(defun cmpgrad (n x grad fsum ffun h typx)
  (let ((f (fundata-f ffun))
	(arg (fundata-arg ffun))
	(cs (fundata-changesign ffun)))
    (dotimes (i n)
      (let ((old-xi (aref x i))
	    (hi (if typx (* (aref typx i) h) h))
	    (f1 0.0)
	    (f2 0.0))
	(setf (aref x i) (+ old-xi hi))
	(setf f1 (get-function-value f x arg cs))
	(setf (aref x i) (- old-xi hi))
	(setf f2 (get-function-value f x arg cs))
	(setf (aref x i) old-xi)
	(setf (aref grad i) (/ (- f1 f2) (* 2.0 hi)))
	(setf (aref fsum i) (+ f1 f2))))))

(defun cmphess (n x hess f fsum ffun h typx)
  (let ((fun (fundata-f ffun))
	(arg (fundata-arg ffun))
	(cs (fundata-changesign ffun)))
    (dotimes (i n)
      (let ((hi (if typx (* (aref typx i) h) h))
	    (old-xi (aref x i)))
	(setf (aref hess i i) (/ (- (aref fsum i) (* 2 f)) (* hi hi)))
	(dotimes (j i)
          (let ((hj (if typx (* (aref typx j) h) h))
		(old-xj (aref x j))
		(f1 0.0)
		(f2 0.0))
	    (setf (aref x i) (+ old-xi hi))
	    (setf (aref x j) (+ old-xj hj))
	    (setf f1 (get-function-value fun x arg cs))
	    (setf (aref x i) (- old-xi hi))
	    (setf (aref x j) (- old-xj hj))
	    (setf f2 (get-function-value fun x arg cs))
	    (setf (aref x i) old-xi)
	    (setf (aref x j) old-xj)
	    (let ((hij (/ (- (+ (* 2 f) f1 f2) (aref fsum i) (aref fsum j))
			  (* 2.0 hi hj))))
	      (setf (aref hess i j) hij)
	      (setf (aref hess j i) hij))))))))    

(defun numgrad (f x &optional scale h all cs)
  (let* ((n (length x))
	 (fd (make-function-data f n cs h scale))
	 (xv (make-array n :initial-contents x))
	 (grad (make-array n))
	 (fv (eval-function fd xv grad nil)))
    (coerce grad 'list)))

(defun numhess (f x &optional scale h all cs)
  (let* ((n (length x))
	 (fd (make-function-data f n cs h scale))
	 (xv (make-array n :initial-contents x))
	 (grad (make-array n))
	 (hess (make-array (list n n)))
	 (fv (eval-function fd xv grad hess)))
    (if all (list fv (coerce grad 'list) hess) hess)))


;;;;
;;;; Nonlinear optimization modules adapted from Dennis and Schnabel, 
;;;; "Numerical Methods for Unconstrained Optimization and Nonlinear
;;;; Equations."
;;;;


;;;;
;;;; Mode Internals Data
;;;;

(defmacro make-internals () `(make-array 7))
(defmacro internals-f (i) `(aref ,i, 0))
(defmacro internals-x (i) `(aref ,i, 1))
(defmacro internals-scale (i) `(aref ,i, 2))
(defmacro internals-delf (i) `(aref ,i, 3))
(defmacro internals-hessf (i) `(aref ,i, 4))
(defmacro internals-ipars (i) `(aref ,i, 5))
(defmacro internals-dpars (i) `(aref ,i, 6))

(defmacro make-ipars () `(make-array 11))
(defmacro ipars-n (ipars) `(aref ,ipars 0))
(defmacro ipars-maxiter (i) `(aref ,i 1))
(defmacro ipars-backtrack (i) `(aref ,i 2))
(defmacro ipars-verbose (i) `(aref ,i 3))
(defmacro ipars-vals-suppl (i) `(aref ,i 4))
(defmacro ipars-count (i) `(aref ,i 5))
(defmacro ipars-termcode (i) `(aref ,i 6))
(defmacro ipars-change-sign (i) `(aref ,i 7))
(defmacro ipars-maxtaken (i) `(aref ,i 8))
(defmacro ipars-consecmax (i) `(aref ,i 9))
(defmacro ipars-retcode (i) `(aref ,i 10))

(defmacro make-dpars () `(make-array 8))
(defmacro dpars-typf(d) `(aref ,d 0))
(defmacro dpars-derivstep (d) `(aref ,d 1))
(defmacro dpars-gradtol (d) `(aref ,d 2))
(defmacro dpars-steptol (d) `(aref ,d 3))
(defmacro dpars-maxstep (d) `(aref ,d 4))
(defmacro dpars-hessadd (d) `(aref ,d 5))
(defmacro dpars-f (d) `(aref ,d 6))
(defmacro dpars-new-f (d) `(aref ,d 7))

(defun newinternals (f x scale h)
  (let ((n (length x))
	(ip (make-ipars))
	(dp (make-dpars))
	(internals (make-internals)))
    (setf (ipars-n ip) n)
    (setf (ipars-maxiter ip ) -1)
    (setf (ipars-backtrack ip) t)
    (setf (ipars-verbose ip) 0)
    (setf (ipars-vals-suppl ip) nil)
    (setf (ipars-count ip) 0)
    (setf (ipars-termcode ip) 0)
    (setf (ipars-change-sign ip) t)

    (setf (dpars-typf dp) 1.0)
    (setf (dpars-derivstep dp) h)
    (setf (dpars-gradtol dp) -1.0)
    (setf (dpars-steptol dp) -1.0)
    (setf (dpars-maxstep dp) -1.0)
    (setf (dpars-hessadd dp) 0.0)

    (setf (internals-f internals) f)
    (setf (internals-x internals)
	  (make-array n :element-type 'float :initial-contents x))
    (setf (internals-scale internals)
	  (if scale
	      (make-array n :element-type 'float :initial-contents scale)
	      (make-array n :element-type 'float :initial-element 1.0)))
    (setf (internals-ipars internals) ip)
    (setf (internals-dpars internals) dp)
    (setf (internals-delf internals) (make-array n :element-type 'float))
    (setf (internals-hessf internals)
	  (make-array (list n n) :element-type 'float))
    internals))


;;;;
;;;; Definitions and Globals
;;;;

(defparameter *init-grad-frac* 0.001)
(defparameter *consec-max-limit* 5)
(defparameter *alpha* 0.0001)
(defparameter *max-step-factor* 1000)
(defparameter *gradtol-power* (/ 1.0 3.0))
(defparameter *steptol-power* (/ 2.0 3.0))
(defparameter *itnlimit* 100)
(defparameter *verbose* 0)
(defparameter *use-search* t)

(defparameter *termcodes*
  (vector "not yet terminated"
	  "gradient size is less than gradient tolerance"
	  "step size is less than step tolerance"
	  "no satisfactory step found in backtracking"
	  "iteration limit exceeded"
	  "maximum size step taken 5 iterations in a row"))


;;;;
;;;; Iteration Data
;;;;
                            
(defmacro make-iteration () `(make-array 12))
(defmacro iteration-ffun (i) `(aref ,i 0))
(defmacro iteration-x (i) `(aref ,i 1))
(defmacro iteration-new-x (i) `(aref ,i 2))
(defmacro iteration-sx (i) `(aref ,i 3))
(defmacro iteration-delf (i) `(aref ,i 4))
(defmacro iteration-new-delf (i) `(aref ,i 5))
(defmacro iteration-qnstep (i) `(aref ,i 6))
(defmacro iteration-L (i) `(aref ,i 7))
(defmacro iteration-hessf (i) `(aref ,i 8))
(defmacro iteration-work (i) `(aref ,i 9))
(defmacro iteration-ip (i) `(aref ,i 10))
(defmacro iteration-dp (i) `(aref ,i 11))

(defmacro iteration-n (i) `(ipars-n (iteration-ip ,i)))
(defmacro iteration-itnlimit (i) `(ipars-maxiter (iteration-ip ,i)))
(defmacro iteration-backtrack (i) `(ipars-backtrack (iteration-ip ,i)))
(defmacro iteration-verbose (i) `(ipars-verbose (iteration-ip ,i)))
(defmacro iteration-vals-suppl (i) `(ipars-vals-suppl (iteration-ip ,i)))
(defmacro iteration-count (i) `(ipars-count (iteration-ip ,i)))
(defmacro iteration-termcode (i) `(ipars-termcode (iteration-ip ,i)))
(defmacro iteration-maxtaken (i) `(ipars-maxtaken (iteration-ip ,i)))
(defmacro iteration-consecmax (i) `(ipars-consecmax (iteration-ip ,i)))
(defmacro iteration-retcode (i) `(ipars-retcode (iteration-ip ,i)))
(defmacro iteration-change-sign (i) `(ipars-change-sign (iteration-ip ,i)))
(defmacro iteration-typf (i) `(dpars-typf (iteration-dp ,i)))
(defmacro iteration-gradtol (i) `(dpars-gradtol (iteration-dp ,i)))
(defmacro iteration-steptol (i) `(dpars-steptol (iteration-dp ,i)))
(defmacro iteration-maxstep (i) `(dpars-maxstep (iteration-dp ,i)))
(defmacro iteration-hessadd (i) `(dpars-hessadd (iteration-dp ,i)))
(defmacro iteration-f (i) `(dpars-f (iteration-dp ,i)))
(defmacro iteration-new-f (i) `(dpars-new-f (iteration-dp ,i)))


;;;;
;;;; Stopping Criteria
;;;;

(defun gradsize (iter new)
  (let ((f (iteration-f iter))
	(typf (iteration-typf iter))
	(x (iteration-x iter))
	(sx (iteration-sx iter))
	(delf (if new (iteration-new-delf iter) (iteration-delf iter))))
    (uncmin-gradsize delf x sx (max (abs f) typf))))

(defun incrsize (iter)
  (let ((n (iteration-n iter))
	(new-x (iteration-new-x iter))
	(x (iteration-x iter))
	(sx (iteration-sx iter))
	(work (iteration-work iter)))
    (blas-dcopy n x 0 1 work 0 1)
    (blas-daxpy n -1.0 new-x 0 1 work 0 1)
    (uncmin-maxrelsize work x sx)))

(defun stoptest0 (iter)
  (setf (iteration-consecmax iter) 0)
  (setf (iteration-termcode iter)
	(if (<= (gradsize iter nil)
		(* *init-grad-frac* (iteration-gradtol iter)))
	    1
	    0))
  (iteration-termcode iter))

(defun stoptest (iter)
  (let ((retcode (iteration-retcode iter))
	(gradtol (iteration-gradtol iter))
	(steptol (iteration-steptol iter))
	(count (iteration-count iter))
	(itnlimit (iteration-itnlimit iter))
	(maxtaken (iteration-maxtaken iter))
	(consecmax (iteration-consecmax iter))
	(termcode 0))
    (cond
     ((= retcode 1) (setf termcode 3))
     ((<= (gradsize iter t) gradtol) (setf termcode 1))
     ((<= (incrsize iter) steptol) (setf termcode 2))
     ((>= count itnlimit) (setf termcode 4))
     (maxtaken (incf consecmax)
	       (if (>= consecmax *consec-max-limit*) (setf termcode 5)))
     (t (setf consecmax 0)))
    
    (setf (iteration-consecmax iter) consecmax)
    (setf (iteration-termcode iter) termcode)
    termcode))


;;;;
;;;; Function and Derivative Evaluation
;;;;

(defun eval-funval (iter)
  (setf (iteration-f iter)
	(eval-function (iteration-ffun iter) (iteration-x iter) nil nil)))

(defun eval-next-funval (iter)
  (setf (iteration-new-f iter)
	(eval-function (iteration-ffun iter) (iteration-new-x iter) nil nil)))

(defun eval-gradient (iter)
  (eval-function (iteration-ffun iter)
		 (iteration-x iter)
		 (iteration-delf iter)
		 nil))

(defun eval-next-gradient (iter)
  (eval-function (iteration-ffun iter)
		 (iteration-new-x iter)
		 (iteration-new-delf iter)
		 nil))

(defun eval-hessian (iter)
  (eval-function (iteration-ffun iter)
		 (iteration-x iter)
		 nil
		 (iteration-hessf iter)))


;;;;
;;;; Backtracking Line Search
;;;;
;;;; Modified to quit after *itnlimit* steps in case regular termination
;;;; doesn't work (because of NaN's).

(defun linesearch (iter)
  (let ((n (iteration-n iter))
	(x (iteration-x iter))
	(new-x (iteration-new-x iter))
	(qnstep (iteration-qnstep iter)))
    (cond
     ((not (iteration-backtrack iter))
      (setf (iteration-maxtaken iter) nil)
      (blas-dcopy n x 0 1 new-x 0 1)
      (blas-daxpy n 1.0 qnstep 0 1 new-x 0 1)
      (eval-next-funval iter)
      (setf (iteration-retcode iter) 0))
     (t
      (let ((maxstep (iteration-maxstep iter))
	    (delf (iteration-delf iter))
	    (sx (iteration-sx iter))
	    (work (iteration-work iter)))
	(setf (iteration-maxtaken iter) nil)
	(setf (iteration-retcode iter) 2)
    
	(dotimes (i n)
	  (setf (aref work i) (* (aref sx i) (aref qnstep i))))
	(let ((newtlen (blas-dnrm2 n work 0 1)))
	  (when (> newtlen maxstep)
	        (dotimes (i n)
		  (setf (aref qnstep i)
			(* (aref qnstep i) (/ maxstep newtlen))))
		(setf newtlen maxstep))
	  (let* ((initslope (blas-ddot n delf 0 1 qnstep 0 1))
		 (rellength (uncmin-maxrelsize qnstep x sx))
		 (minlambda (if (= rellength 0.0)
				2.0
			        (/ (iteration-steptol iter) rellength)))
		 (lambda 1.0)
		 (lambdaprev 1.0)
		 (fprev 1.0)
		 (count 0))
	    (loop
	     (incf count)
	     (when (<= (iteration-retcode iter) 1) (return))
	     (blas-dcopy n x 0 1 new-x 0 1)
	     (blas-daxpy n lambda qnstep 0 1 new-x 0 1)
	     (eval-next-funval iter)
	     (cond
	      ((<= (iteration-new-f iter)
		   (+ (iteration-f iter) (* *alpha* lambda initslope)))
	       (setf (iteration-retcode iter) 0)
	       (when (and (= lambda 1.0) (> newtlen (* 0.99 maxstep)))
		     (setf (iteration-maxtaken iter) t)))
	      ((or (< lambda minlambda) (>= count *itnlimit*))
	       (setf (iteration-retcode iter) 1)
	       (setf (iteration-new-f iter) (iteration-f iter))
	       (blas-dcopy n x 0 1 new-x 0 1))
	      (t
	       (setf lambdatemp
		     (uncmin-linesearch (iteration-f iter) initslope
					lambda (iteration-new-f iter)
					lambdaprev fprev))
	       (setf lambdaprev lambda)
	       (setf fprev (iteration-new-f iter))
	       (setf lambda (max (* 0.1 lambda) lambdatemp))
	       (when (> (iteration-verbose iter) 0)
		     (format t "Backtracking: lambda = ~,6g~%"
			     lambda))))))))))))


;;;;
;;;; Status Printing Functions
;;;;

(defun print-header (iter)
  (when (< 0 (iteration-verbose iter))
	(format t "Iteration ~d.~%" (iteration-count iter))))

(defun print-status (iter)
  (let ((n (iteration-n iter))
	(f (iteration-f iter))
	(x (iteration-x iter))
	(delf (iteration-delf iter))
	(hessf (iteration-hessf iter))
	(verbose (iteration-verbose iter))
	(change-sign (iteration-change-sign iter))
	(tcode (iteration-termcode iter)))
    (when (< 0 verbose)
	  (format t "Criterion value = ~,6g~%" (if change-sign (- f) f))
	  (when (< 1 verbose)
		(format t "Location = <")
		(dotimes (i n)
		  (format t (if (< i (- n 1)) "~,6g " "~,6g>~%") (aref x i)))
		(when (< 2 verbose)
		      (format t "Gradient = <")
		      (dotimes (i n)
			(let ((di (aref delf i)))
			  (format t
				  (if (< i (- n 1)) "~,6g " "~,6g>~%")
				  (if change-sign (- di) di))))
		      (when (< 3 verbose)
			    (format t "Hessian:~%")
			    (dotimes (i n)
			      (dotimes (j n)
			        (let ((hij (aref hess (+ (* i n) j))))
				  (format t
					  (if (< j (- n 1)) "~,6g " "~,6g~%")
					  (if change-sign (- hij) hij))))))))
	  (when (/= tcode 0)
		(format t
			"Reason for termination: ~a.~%"
			(aref *termcodes* tcode))))))


;;;;
;;;; Iteration Driver
;;;;

(defun findqnstep (iter)
  (let ((sx (iteration-sx iter))
	(hessf (iteration-hessf iter))
	(L (iteration-L iter))
	(delf (iteration-delf iter))
	(qnstep (iteration-qnstep iter)))
    (setf (iteration-hessadd iter) (uncmin-modelhess sx hessf L))
    (uncmin-cholsolve delf L  qnstep)))

(defun iterupdate (iter)
  (let ((n (iteration-n iter)))
    (setf (iteration-f iter) (iteration-new-f iter))
    (blas-dcopy n (iteration-new-x iter) 0 1 (iteration-x iter) 0 1)
    (blas-dcopy n (iteration-new-delf iter) 0 1 (iteration-delf iter) 0 1)))


;;;;
;;;; External Interface Routines
;;;;

(defun mindriver (iter trfun)
  (setf (iteration-consecmax iter) 0)
  (setf (iteration-count iter) 0)
  (setf (iteration-termcode iter) 0)
  (unless (iteration-vals-suppl iter)
	  (eval-funval iter)
	  (eval-gradient iter)
	  (eval-hessian iter))

  (stoptest0 iter)
  (print-header iter)
  (print-status iter)
  (loop
   (when (/= 0 (iteration-termcode iter)) (return))
   (incf (iteration-count iter))
   (print-header iter)
   (findqnstep iter)
   (linesearch iter)
   (eval-next-gradient iter)
   (stoptest iter)
   (iterupdate iter)
   (eval-hessian iter)
   (print-status iter)
   (when trfun (funcall trfun))))

(defun minresultstring (code)
  (if (<= 0 code (- (length *termcodes*) 1))
      (aref *termcodes* code)
      "unknown return code"))
  
(defun minsetup (ffun x typx ip dp delf hessf)
  (let ((iter (make-iteration))
	(n (ipars-n ip)))
    (setf (iteration-ip iter) ip)
    (setf (iteration-dp iter) dp)

    (setf (iteration-ffun iter) ffun)
    (setf (iteration-x iter) x)
    (setf (iteration-new-x iter) (make-array n :element-type 'float))
    (setf (iteration-sx iter)
	  (make-array n :element-type 'float :initial-element 1.0))
    (setf (iteration-delf iter) delf)
    (setf (iteration-new-delf iter) (make-array n :element-type 'float))
    (setf (iteration-qnstep iter) (make-array n :element-type 'float))

    (when typx
	  (let ((sx (iteration-sx iter)))
	    (dotimes (i n)
	      (let ((txi (aref typx i)))
		(if (< 0.0 txi)
		    (setf (aref sx i) (/ 1.0 txi)))))))

    (setf (iteration-L iter) (make-array (list n n) :element-type 'float))
    (setf (iteration-hessf iter) hessf)
    (setf (iteration-work iter) (make-array n :element-type 'float))

    (when (< (iteration-verbose iter) 0)
	  (setf (iteration-verbose iter) *verbose*))
    (when (< (iteration-itnlimit iter) 0)
	  (setf (iteration-itnlimit iter) *itnlimit*))

    (when (<= (iteration-typf iter) 0.0)
	  (setf (iteration-typf iter) 1.0))
    (when (<= (iteration-gradtol iter) 0.0)
	  (setf (iteration-gradtol iter) (^ machine-epsilon *gradtol-power*)))
    (when (<= (iteration-steptol iter) 0.0)
	  (setf (iteration-steptol iter) (^ machine-epsilon *steptol-power*)))

    (when (<= (iteration-maxstep iter) 0.0)
	  (let ((dx (iteration-x iter))
		(sx (iteration-sx iter))
		(w (iteration-work iter)))
	    (dotimes (i n)
	      (setf (aref w i) (* (aref dx i) (aref sx i))))
	    (let ((nx0 (blas-dnrm2 n w 0 1))
		  (nsx (blas-dnrm2 n sx 0 1)))
	      (setf (iteration-maxstep iter)
		    (* *max-step-factor* (max nx0 nsx))))))
    iter))
;;;;
;;;; Mode Info Accessors
;;;;

(defmacro mode-info-f () `(aref (slot-value 'internals) 0))
(defmacro mode-info-x () `(aref (slot-value 'internals) 1))
(defmacro mode-info-scale () `(aref (slot-value 'internals) 2))
(defmacro mode-info-delf () `(aref (slot-value 'internals) 3))
(defmacro mode-info-hessf () `(aref (slot-value 'internals) 4))
(defmacro mode-info-ipars () `(aref (slot-value 'internals) 5))
(defmacro mode-info-dpars () `(aref (slot-value 'internals) 6))

(defmacro ipars-n (ipars) `(aref ,ipars 0))
(defmacro ipars-maxiter (ipars) `(aref ,ipars 1))
(defmacro ipars-backtrack (ipars) `(aref ,ipars 2))
(defmacro ipars-verbose (ipars) `(aref ,ipars 3))

(defmacro dpars-derivstep (dpars) `(aref ,dpars 1))
(defmacro dpars-f (dpars) `(aref ,dpars 6))


;;;;
;;;; Mode Info Prototype
;;;;

(defproto minfo-proto '(internals))

(defmeth minfo-proto :isnew (f x &key scale (derivstep -1.0))
  (setf (slot-value 'internals) (newinternals f x scale derivstep))
  nil)

(defmeth minfo-proto :maximize (&optional verbose trfun)
  (let* ((internals (slot-value 'internals))
	 (f (internals-f internals))
	 (x (internals-x internals))
	 (scale (internals-scale internals))
	 (ip (internals-ipars internals))
	 (dp (internals-dpars internals))
	 (delf (internals-delf internals))
	 (hessf (internals-hessf internals))
	 (n (ipars-n ip))
	 (func (make-function-data f n t (dpars-derivstep dp) scale))
	 (iter (minsetup func x scale ip dp delf hessf)))
    (when (and verbose (integerp verbose))
	  (setf (ipars-verbose ip) verbose))
    (when (> (ipars-verbose ip) 0)
	  (format t "maximizing...~%"))
    (mindriver iter trfun)
    (setf (ipars-vals-suppl ip) t)
    (minresultstring (ipars-termcode ip))))

(defmeth minfo-proto :x () (coerce (mode-info-x) 'list))
(defmeth minfo-proto :scale () (coerce (mode-info-scale) 'list))
(defmeth minfo-proto :derivstep () (dpars-derivstep (mode-info-dpars)))

(defmeth minfo-proto :f (&optional (val nil set))
  (when set
	(send self :set-no-vals-supplied)
	(setf (mode-info-f) val))
  (mode-info-f))
	
(defmeth minfo-proto :fvals ()
  (let* ((n (ipars-n (mode-info-ipars)))
	 (val (dpars-f (mode-info-dpars)))
	 (grad (coerce (mode-info-delf) 'list))
	 (hess (make-array (list n n))))
    (replace (compound-data-seq hess)
	     (compound-data-seq (mode-info-hessf)))
    (list val grad hess)))

(defmeth minfo-proto :copy ()
  (let ((obj (make-object minfo-proto))
	(internals (copy-seq (slot-value 'internals))))
    (dotimes (i (length internals))
	     (let ((x (aref internals i)))
	       (if (sequencep x)
		   (setf (aref internals i) (copy-seq x)))))
    (send obj :add-slot 'internals internals)
    obj))

(defmeth minfo-proto :derivscale ()
  (let* ((x (send self :x))
	 (step (^ machine-epsilon (/ 1 6)))
	 (hess (numhess (send self :f) x (send self :scale) step))
	 (scale (pmax (abs x) (sqrt (abs (/ (diagonal hess)))))))
    (setf hess (numhess (send self :f) x scale step))
    (setf scale (pmax (abs x) (sqrt (abs (/ (diagonal hess))))))
    (setf (mode-info-scale) (coerce scale '(vector float)))
    (setf (dpars-derivstep (mode-info-dpars)) step)))

(defmeth minfo-proto :verbose (&optional (val nil set))
  (when set
	(setf (ipars-verbose (mode-info-ipars))
	      (cond ((integerp val) val)
		    ((null val) 0)
		    (t 1))))
  (ipars-verbose (mode-info-ipars)))

(defmeth minfo-proto :backtrack (&optional (val nil set))
  (if set (setf (ipars-backtrack (mode-info-ipars)) (if val 1 0)))
  (ipars-backtrack (mode-info-ipars)))

(defmeth minfo-proto :maxiter (&optional (val nil set))
    (if set (setf (ipars-maxiter (mode-info-ipars)) 
		  (if (integerp val) val -1)))
    (ipars-maxiter (mode-info-ipars)))


;;;;
;;;;
;;;; Newton's Method with Backtracking
;;;;
;;;;

(defun newtonmax (f start &key 
                    scale 
                    (derivstep -1.0)
                    (count-limit -1)
                    (verbose 1)
                    return-derivs
		    trace)
"Args:(f start &key scale derivstep (verbose 1) return-derivs)
Maximizes F starting from START using Newton's method with backtracking.
If RETURN-DERIVS is NIL returns location of maximum; otherwise returns
list of location, unction value, gradient and hessian at maximum.
SCALE should be a list of the typical magnitudes of the parameters.
DERIVSTEP is used in numerical derivatives and VERBOSE controls printing
of iteration information. COUNT-LIMIT limits the number of iterations"
  (let ((verbose (if verbose (if (integerp verbose) verbose 1) 0))
        (minfo (send minfo-proto :new f start 
                     :scale scale :derivstep derivstep)))
    (send minfo :maxiter count-limit)
    (send minfo :derivscale)
    (send minfo :maximize
	  verbose
	  (if trace #'(lambda () (funcall trace minfo))))
    (if return-derivs
        (cons (send minfo :x) (- (send minfo :fvals)))
        (send minfo :x))))

;;;;
;;;;
;;;; Nelder-Mead Simplex Method
;;;;
;;;;

(defun nelmeadmax (f start &key 
                     (size 1)
                     (epsilon (sqrt machine-epsilon)) 
                     (count-limit 500)
                     (verbose t)
                     (alpha 1.0) 
                     (beta 0.5) 
                     (gamma 2.0)
                     (delta 0.5))
"Args: (f start &key (size 1) (epsilon (sqrt machine-epsilon)) 
          (count-limit 500) (verbose t) alpha beta gamma delta)
Maximizes F using the Nelder-Mead simplex method. START can be a
starting simplex - a list of N+1 points, with N=dimension of problem,
or a single point. If start is a single point you should give the
size of the initial simplex as SIZE, a sequence of length N. Default is
all 1's. EPSILON is the convergence tolerance. ALPHA-DELTA can be used to
control the behavior of simplex algorithm."
    (let ((s (send simplex-proto :new f start size)))
      (do ((best (send s :best-point) (send s :best-point))
           (count 0 (+ count 1))
           next)
          ((or (< (send s :relative-range) epsilon) (>= count count-limit))
           (if (and verbose (>= count count-limit))
               (format t "Iteration limit exceeded.~%"))
           (send s :point-location (send s :best-point)))
          (setf next (send s :extrapolate-from-worst (- alpha)))
          (if (send s :is-worse best next)
              (setf next (send s :extrapolate-from-worst gamma))
              (when (send s :is-worse next (send s :second-worst-point))
                    (setf next (send s :extrapolate-from-worst beta))
                    (if (send s :is-worse next (send s :worst-point))
                        (send s :shrink-to-best delta))))
          (if verbose 
              (format t "Value = ~g~%" 
                      (send s :point-value (send s :best-point)))))))
          

;;;
;;; Simplex Prototype
;;;

(defproto simplex-proto '(f simplex))

;;;
;;; Simplex Points
;;;

(defmeth simplex-proto :make-point (x)
  (let ((f (send self :f)))
    (if f 
        (let ((val (funcall f x)))
          (cons (if (consp val) (car val) val) x))
        (cons nil x))))

(defmeth simplex-proto :point-value (x) (car x))

(defmeth simplex-proto :point-location (x) (cdr x))

(defmeth simplex-proto :is-worse (x y)
  (< (send self :point-value x) (send self :point-value y)))

;;;
;;; Making New Simplices
;;;

(defmeth simplex-proto :isnew (f start &optional size)
  (send self :simplex start size)
  (send self :f f))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth simplex-proto :simplex (&optional new size)
  (if new
      (let ((simplex 
             (if (and (consp new) (sequencep (car new)))
                 (if (/= (length new) (+ 1 (length (car new))))
                     (error "bad simplex data")
                     (copy-list new))
                 (let* ((n (length new))
                        (size (if size size (repeat 1 n)))
                     ;   (pts (- (* 2 (uniform-rand (repeat n (+ n 1)))) 1)))
                        (diag (* 2 size (- (random (repeat 2 n)) .5)))
                     	(pts (cons (repeat 0 n)	
                                   (mapcar #'(lambda (x) (coerce x 'list))
                                           (column-list (diagonal diag))))))
                   (mapcar #'(lambda (x) (+ (* size x) new)) pts)))))
        (setf (slot-value 'simplex) 
              (mapcar #'(lambda (x) (send self :make-point x)) simplex))
        (send self :sort-simplex)))
  (slot-value 'simplex))

(defmeth simplex-proto :f (&optional f)
  (when f
        (setf (slot-value 'f) f)
        (let ((simplex 
               (mapcar #'(lambda (x) (send self :point-location x))
                       (send self :simplex))))
          (send self :simplex simplex)))
  (slot-value 'f))

(defmeth simplex-proto :sort-simplex ()
  (if (send self :f)
      (setf (slot-value 'simplex) 
            (sort (slot-value 'simplex)
                  #'(lambda (x y) (send self :is-worse x y))))))

;;;
;;; Other Methods Using List Representation of SImplex
;;;

(defmeth simplex-proto :best-point () (car (last (send self :simplex))))
(defmeth simplex-proto :worst-point () (first (send self :simplex)))
(defmeth simplex-proto :second-worst-point () (second (send self :simplex)))
(defmeth simplex-proto :replace-point (new old)
  (let* ((simplex (send self :simplex))
         (n (position old simplex)))
    (when n 
          (setf (nth n simplex) new)
          (send self :sort-simplex))))
(defmeth simplex-proto :mean-opposite-face (x)
  (let ((face (mapcar #'(lambda (x) (send self :point-location x))
                      (remove x (send self :simplex)))))
    (/ (apply #'+ face) (length face))))

;;;
;;; Iteration Step Methods
;;;

(defmeth simplex-proto :extrapolate-from-worst (fac)
  (let* ((worst (send self :worst-point))
         (wloc (send self :point-location worst))
         (delta (- (send self :mean-opposite-face worst) wloc))
         (new (send self :make-point (+ wloc (* (- 1 fac) delta)))))
    (if (send self :is-worse worst new) (send self :replace-point new worst))
    new))

(defmeth simplex-proto :shrink-to-best (fac)
  (let* ((best (send self :best-point))
         (bloc (send self :point-location best)))
    (dolist (x (copy-list (send self :simplex)))
            (if (not (eq x best))
                (send self :replace-point 
                      (send self :make-point 
                            (+ bloc 
                               (* fac 
                                  (- (send self :point-location x) bloc))))
                      x)))))

(defmeth simplex-proto :relative-range ()
  (let ((best (send self :point-value (send self :best-point)))
        (worst (send self :point-value (send self :worst-point))))
    (* 2 (/ (abs (- best worst)) (+ 1 (abs best) (abs worst))))))
