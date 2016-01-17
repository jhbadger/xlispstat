(in-package :xlisp)
(export 'rationalize)
(defun rationalize (val)	; hopefully readable conversion
       (unless (typep val 'flonum)
	       (if (typep val 'rational)
		   (return-from rationalize val)
		   (error "~s is invalid type" val)))
       (let ((fraction (abs (rem val 1.0))))
	    (if (zerop fraction) 
		(round val)
		(let ((limit (expt 10 (- (+ 7 (truncate (log fraction 10)))
					  (max 0 (truncate (log (abs val) 10))))))
		      divisor)
		     (cond ((>= limit 10000)	; allow primes 3 3 7 11 13
			    (setq limit (* 9009 (/ limit 10000))))
			   ((>= limit 1000)	; allow primes 3 3 7 11
			    (setq limit (* 693 (/ limit 1000))))
			   ((>= limit 100)	; allow primes 3 3 7
			    (setq limit (* 63 (/ limit 100)))))
		     (setq divisor (round (/ limit fraction)))
		      (if (floatp divisor) 
			  (round val)	; Doesn't fit
			  (/ (round (* val divisor)) divisor))))))
