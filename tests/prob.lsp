; test probability distributions and random number generators
; should do more extensive integration test

(setf eps 2.e-4)

(flet ((ptest (prob quant rand n &rest args)
         (let* ((x (apply rand n args))
		(x1 (apply quant (apply prob x args) args)))
           (check #'< (abs (- x1 x)) (* eps (max (abs x)))))))
  (ptest #'beta-cdf #'beta-quant #'beta-rand 20 2 3)
  (ptest #'cauchy-cdf #'cauchy-quant #'cauchy-rand 20)      
  (ptest #'chisq-cdf #'chisq-quant #'chisq-rand 20 2)      
  (ptest #'gamma-cdf #'gamma-quant #'gamma-rand 20 5)
  (ptest #'normal-cdf #'normal-quant #'normal-rand 20)
  (ptest #'t-cdf #'t-quant #'t-rand 20 5)
  (ptest #'identity #'identity #'uniform-rand 20)
  (ptest #'f-cdf #'f-quant #'f-rand 20 2 3))

(labels ((trapezoid (from to n f args)
           (let* ((x (rseq from to n))
		  (w (append '(.5) (repeat 1 (- n 2)) '(.5)))
		  (fvals (apply f x args)))
             (* (/ (- to from) (- n 1))
		(sum (* fvals w)))))
	 (int-test (dens quant &rest args)
	   (check #'<
		  (abs (- (trapezoid (apply quant .01 args)
				     (apply quant .99 args)
				     500
				     dens
				     args)
			  .98))
		  eps)))
  (int-test #'beta-dens #'beta-quant 1.5 3)
  (int-test #'cauchy-dens #'cauchy-quant)
  (int-test #'chisq-dens #'chisq-quant 3)
  (int-test #'gamma-dens #'gamma-quant 5)
  (int-test #'normal-dens #'normal-quant)
  (int-test #'t-dens #'t-quant 3)
  (int-test #'f-dens #'f-quant 7 45))
