(setf eps 1e-10)
(setf dx (- (iseq 1 10) 5))
(setf dy (* dx dx))
(setf zx (complex dx dy))
(setf zy (complex dy dx))

;(defun test-report (name val) (format t "~&~a: ~9t~f~%" name val))
(defun test-report (name val) (check #'< val eps))

(defun bv (n x incx)
  (if (< incx 0)
      (reverse (select x (* (abs incx) (iseq n))))
      (select x (* incx (iseq n)))))


;;;;
;;;; _ASUM
;;;;

(defun asum (n x incx)
  (let ((x (bv n x incx)))
    (sum (abs (realpart x)) (abs (imagpart x)))))

(let ((x (coerce dx '(vector c-double)))
      (n 5)
      (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (let ((val (blas-dasum n x 0 incx))
	  (v (asum n x incx)))
      (setf maxdiff (max maxdiff (abs (- val v))))))
  (test-report "DASUM" maxdiff))

(let ((x (coerce (complex dx (* 2 dx)) '(vector c-dcomplex)))
      (n 5)
      (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (let ((val (blas-dzasum n x 0 incx))
	  (v (asum n x incx)))
      (setf maxdiff (max maxdiff (abs (- val v))))))
  (test-report "DZASUM" maxdiff))


;;;;
;;;; _AXPY
;;;;

(defun axpy (n a x incx y incy)
  (let ((x (bv n x incx))
	(y (bv n y incy)))
    (+ (* a x) y)))

(let ((x (coerce dx '(vector c-double)))
      (y (coerce dy '(vector c-double)))
      (a 3.0)
      (n 5)
      (val nil)
      (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (dolist (incy '(1 2 -1 -2))
      (setf val (copy-array y))
      (blas-daxpy n a x 0 incx val 0 incy)
      (let ((diff (max (abs (- (bv n val incy) (axpy n a x incx y incy))))))
	(setf maxdiff (max maxdiff diff)))))
  (test-report "DAXPY" maxdiff))

(let ((x (coerce (complex dx (* 2 dx)) '(vector c-dcomplex)))
      (y (coerce (complex dy (* 2 dy)) '(vector c-dcomplex)))
      (a (complex 3.0 6.0))
      (n 5)
      (val nil)
      (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (dolist (incy '(1 2 -1 -2))
      (setf val (copy-array y))
      (blas-zaxpy n a x 0 incx val 0 incy)
      (let ((diff (max (abs (- (bv n val incy) (axpy n a x incx y incy))))))
	(setf maxdiff (max maxdiff diff)))))
  (test-report "ZAXPY" maxdiff))


;;;;
;;;; _DOT_
;;;;

(defun dot (n x incx y incy &optional (conj nil))
  (let ((x (bv n x incx))
	(y (bv n y incy)))
    (sum (* (if conj (conjugate x) x) y))))

(let ((x (coerce dx '(vector c-double)))
      (y (coerce dy '(vector c-double)))
      (n 5)
      (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (dolist (incy '(1 2 -1 -2))
      (let ((val (blas-ddot n x 0 incx y 0 incy))
	    (v (dot n x incx y incy)))
	(setf maxdiff (max maxdiff (abs (- val v)))))))
  (test-report "DDOT" maxdiff))

(let ((x (coerce (complex dx (* 2 dx)) '(vector c-dcomplex)))
      (y (coerce (complex dy (* 2 dy)) '(vector c-dcomplex)))
      (n 5)
      (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (dolist (incy '(1 2 -1 -2))
      (let ((val (blas-zdotu n x 0 incx y 0 incy))
	    (v (dot n x incx y incy)))
	(setf maxdiff (max maxdiff (abs (- val v)))))))
  (test-report "ZDOTU" maxdiff))

(let ((x (coerce (complex dx (* 2 dx)) '(vector c-dcomplex)))
      (y (coerce (complex dy (* 3 dy)) '(vector c-dcomplex)))
      (n 5)
      (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (dolist (incy '(1 2 -1 -2))
      (let ((val (blas-zdotc n x 0 incx y 0 incy))
	    (v (dot n x incx y incy t)))
	(setf maxdiff (max maxdiff (abs (- val v)))))))
  (test-report "ZDOTC" maxdiff))

;;;;
;;;; _NRM2
;;;;

(defun nrm2(n x incx)
  (let ((x (bv n x incx)))
    (sqrt (abs (sum (* (conjugate x) x))))))

(let ((x (coerce dx '(vector c-double)))
      (n 5)
      (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (let ((val (blas-dnrm2 n x 0 incx))
	  (v (nrm2 n x incx)))
      (setf maxdiff (max maxdiff (abs (- val v))))))
  (test-report "DNRM2" maxdiff))

(let ((x (coerce (complex dx (* 2 dx)) '(vector c-dcomplex)))
      (n 5)
      (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (let ((val (blas-dznrm2 n x 0 incx))
	  (v (nrm2 n x incx)))
      (setf maxdiff (max maxdiff (abs (- val v))))))
  (test-report "DZNRM2" maxdiff))


;;;;
;;;; _ROT
;;;;

(defun rot (n x incx y incy c s)
  (let ((x (bv n x incx))
	(y (bv n y incy)))
    (values (+ (* c x) (* s y))
	    (- (* c y) (* s x)))))

(let* ((x (coerce dx '(vector c-double)))
       (y (coerce dy '(vector c-double)))
       (c .2)
       (s (sqrt (- 1 (* c c))))
       (n 5)
       (valx nil)
       (valy nil)
       (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (dolist (incy '(1 2 -1 -2))
      (setf valx (copy-array x))
      (setf valy (copy-array y))
      (blas-drot n valx 0 incx valy 0 incy c s)
      (multiple-value-bind
       (vx vy)
       (rot n x incx y incy c s)
       (let ((vvx (bv n valx incx))
	     (vvy (bv n valy incy)))
	 (setf maxdiff (max maxdiff (abs (- vvx vx)) (abs (- vvy vy))))))))
  (test-report "DROT" maxdiff))

(let* ((x (coerce (complex dx (* 2 dx)) '(vector c-dcomplex)))
       (y (coerce (complex dy (* 3 dy)) '(vector c-dcomplex)))
       (c .2)
       (s (sqrt (- 1 (* c c))))
       (n 5)
       (valx nil)
       (valy nil)
       (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (dolist (incy '(1 2 -1 -2))
      (setf valx (copy-array x))
      (setf valy (copy-array y))
      (blas-zdrot n valx 0 incx valy 0 incy c s)
      (multiple-value-bind
       (vx vy)
       (rot n x incx y incy c s)
       (let ((vvx (bv n valx incx))
	     (vvy (bv n valy incy)))
	 (setf maxdiff (max maxdiff (abs (- vvx vx)) (abs (- vvy vy))))))))
  (test-report "ZDROT" maxdiff))


;;;;
;;;; _ROTG
;;;;

(defun drotg (da db)
  (let ((roe (if (> (abs da) (abs db)) da db))
	(scale (+ (abs da) (abs db))))
    (if (= scale 0.0)
	(values 0.0 0.0 1.0 0.0)
        (let* ((r (* scale (sqrt (+ (^ (/ da scale) 2) (^ (/ db scale) 2)))))
	       (r (* (signum roe) r))
	       (c (/ da r))
	       (s (/ db r))
	       (z 1.0))
	  (if (> (abs da) (abs db)) (seft z s))
	  (if (and (>= (abs db) (abs da)) (/= c 0.0)) (setf z (/ 1.0 c)))
	  (values r z c s)))))

(defun zrotg (ca cb)
  (if (= (abs ca) 0.0)
      (values cb cb 0.0 1.0)
      (let* ((scale (+ (abs ca) (abs cb)))
	     (norm (* scale (sqrt (+ (^ (abs (/ ca scale)) 2)
				     (^ (abs (/ cb scale)) 2)))))
	     (alpha (/ ca (abs ca)))
	     (c (/ (abs ca) norm))
	     (s (/ (* alpha (conjugate cb)) norm))
	     (ca (* alpha norm)))
	(values ca cb c s))))

(let ((a 1)
      (b 7))
  (test-report "DROTG"
	       (max (abs (- (multiple-value-list (blas-drotg a b))
			    (multiple-value-list (drotg a b)))))))

(let ((a #c(1 2))
      (b #c(7 3)))
  (test-report "ZROTG"
	       (max (abs (- (multiple-value-list (blas-zrotg a b))
			    (multiple-value-list (zrotg a b)))))))

;;;;
;;;; _SCAL
;;;;

(defun scal (n a x incx)
  (let ((x (bv n x incx)))
    (* a x)))

(let ((x (coerce dx '(vector c-double)))
      (a 3.0)
      (n 5)
      (val nil)
      (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (setf val (copy-array x))
    (blas-dscal n a val 0 incx)
    (let ((v (scal n a x incx))
	  (vv (bv n val incx)))
      (setf maxdiff (max maxdiff (max (abs (- vv v)))))))
  (test-report "DSCAL" maxdiff))

(let ((x (coerce (complex dx (* 2 dx)) '(vector c-dcomplex)))
      (a 3.0)
      (n 5)
      (val nil)
      (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (setf val (copy-array x))
    (blas-zdscal n a val 0 incx)
    (let ((v (scal n a x incx))
	  (vv (bv n val incx)))
      (setf maxdiff (max maxdiff (max (abs (- vv v)))))))
  (test-report "ZDSCAL" maxdiff))

(let ((x (coerce (complex dx (* 2 dx)) '(vector c-dcomplex)))
      (a (complex 3.0 6.0))
      (n 5)
      (val nil)
      (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (setf val (copy-array x))
    (blas-zscal n a val 0 incx)
    (let ((v (scal n a x incx))
	  (vv (bv n val incx)))
      (setf maxdiff (max maxdiff (max (abs (- vv v)))))))
  (test-report "ZSCAL" maxdiff))


;;;;
;;;; _SWAP
;;;;

(let ((x (coerce dx '(vector c-double)))
      (y (coerce dy '(vector c-double)))
      (n 5)
      (valx nil)
      (valy nil)
      (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (dolist (incy '(1 2 -1 -2))
      (setf valx (copy-array x))
      (setf valy (copy-array y))
      (blas-dswap n valx 0 incx valy 0 incy)
      (let ((vx (bv n x incx))
	    (vy (bv n y incy))
	    (vvx (bv n valx incx))
	    (vvy (bv n valy incy)))
	(setf maxdiff
	      (max maxdiff (max (abs (- vvx vy)) (abs (- vvy vx))))))))
  (test-report "DSWAP" maxdiff))

(let ((x (coerce dx '(vector c-dcomplex)))
      (y (coerce dy '(vector c-dcomplex)))
      (n 5)
      (valx nil)
      (valy nil)
      (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (dolist (incy '(1 2 -1 -2))
      (setf valx (copy-array x))
      (setf valy (copy-array y))
      (blas-zswap n valx 0 incx valy 0 incy)
      (let ((vx (bv n x incx))
	    (vy (bv n y incy))
	    (vvx (bv n valx incx))
	    (vvy (bv n valy incy)))
	(setf maxdiff
	      (max maxdiff (max (abs (- vvx vy)) (abs (- vvy vx))))))))
  (test-report "ZSWAP" maxdiff))

;;;;
;;;; I_AMAX
;;;;

(defun iamax (n x incx)
  (let* ((x (bv n x incx))
	 (ax (+ (abs (realpart x)) (abs (imagpart x)))))
    (position (max ax) ax)))

(let ((x (coerce dx '(vector c-double)))
      (n 5)
      (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (let ((val (blas-idamax n x 0 incx))
	  (v (iamax n x incx)))
      (setf maxdiff (max maxdiff (abs (- val v))))))
  (test-report "IDAMAX" maxdiff))

(let ((x (coerce (complex dx (* 2 dx)) '(vector c-dcomplex)))
      (n 5)
      (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (let ((val (blas-izamax n x 0 incx))
	  (v (iamax n x incx)))
      (setf maxdiff (max maxdiff (abs (- val v))))))
  (test-report "IZAMAX" maxdiff))


;;;;
;;;; _COPY
;;;;

(let ((x (coerce dx '(vector c-double)))
      (y (coerce dy '(vector c-double)))
      (n 5)
      (val nil)
      (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (dolist (incy '(1 2 -1 -2))
      (setf val (copy-array y))
      (blas-dcopy n x 0 incx val 0 incy)
      (let ((v (bv n x incx))
	    (vv (bv n val incy)))
	(setf maxdiff (max maxdiff (max (abs (- v vv))))))))
  (test-report "DCOPY" maxdiff))

(let ((x (coerce (complex dx (* 2 dx)) '(vector c-dcomplex)))
      (y (coerce (complex dy (* 2 dy)) '(vector c-dcomplex)))
      (n 5)
      (val nil)
      (maxdiff 0))
  (dolist (incx '(1 2 -1 -2))
    (dolist (incy '(1 2 -1 -2))
      (setf val (copy-array y))
      (blas-zcopy n x 0 incx val 0 incy)
      (let ((v (bv n x incx))
	    (vv (bv n val incy)))
	(setf maxdiff (max maxdiff (max (abs (- v vv))))))))
  (test-report "ZCOPY" maxdiff))

(setf da '(1 2 3 4 5 6))
(setf db '(1 2 3 4 5 6 13 14 15))
(setf dc '(16 17 18 19 20 21))
(setf dx '(7 8 9))
(setf dy '(10 11 12))

(defvar *verbose-tests* nil)

;(defun test-report (name val) (format t "~&~a: ~8t~f~%" name val))


;;;;
;;;; _GEMV
;;;;

(defun gemv (trans alpha a x incx beta y incy)
  (when (/= (abs incx) 1) (error "increment has to be +1 or -1"))
  (when (/= (abs incy) 1) (error "increment has to be +1 or -1"))
  (when (equalp trans "n") (setf a (transpose a)))
  (when (equalp trans "c") (setf a (conjugate a)))
  (let* ((m (array-dimension a 0))
	 (n (array-dimension a 1))
	 (z (make-array m :initial-element 0)))
    (when (minusp incx) (setf x (reverse (subseq x 0 n))))
    (when (minusp incy) (setf y (reverse (subseq y 0 m))))
    (replace z (* beta y))
    (dotimes (i m)
      (dotimes (j n)
        (incf (aref z i) (* alpha (aref a i j) (aref x j)))))
    (if (minusp incy) (reverse z) z)))

(let ((a (coerce da '(vector c-double)))
      (x (coerce dx '(vector c-double)))
      (y (coerce dy '(vector c-double)))
      (val nil)
      (alpha 2)
      (beta 3)
      (maxdiff 0))
  (dolist (trans '("n" "t"))
    (dolist (incx '(1 -1))
      (dolist (incy '(1 -1))
        (setf val (copy-seq y))
	(blas-dgemv trans 3 2 alpha a 0 3 x 0 incx beta val 0 incy)
	(let ((v (gemv trans alpha (matrix '(2 3) a) x incx beta y incy))
	      (vv (if (equalp trans "n") val (subseq val 0 2))))
	  (when *verbose-tests*
		(format t "DGEMV ~a ~2d ~2d: ~f~%"
			trans
			incx
			incy
			(max (abs (- vv v)))))
	  (setf maxdiff (max maxdiff (max (abs (- vv v)))))))))
  (test-report "DGEMV" maxdiff))

(let ((a (coerce (complex da (* 2 da)) '(vector c-dcomplex)))
      (x (coerce (complex dx (* 3 dx)) '(vector c-dcomplex)))
      (y (coerce (complex dy (* 4 dy)) '(vector c-dcomplex)))
      (val nil)
      (alpha (complex 2 4))
      (beta (complex 3 6))
      (maxdiff 0))
  (dolist (trans '("n" "t" "c"))
    (dolist (incx '(1 -1))
      (dolist (incy '(1 -1))
        (setf val (copy-seq y))
	(blas-zgemv trans 3 2 alpha a 0 3 x 0 incx beta val 0 incy)
	(let ((v (gemv trans alpha (matrix '(2 3) a) x incx beta y incy))
	      (vv (if (equalp trans "n") val (subseq val 0 2))))
	  (when *verbose-tests*
		(format t "ZGEMV ~a ~2d ~2d: ~f~%"
			trans
			incx
			incy
			(max (abs (- vv v)))))
	  (setf maxdiff (max maxdiff (max (abs (- vv v)))))))))
  (test-report "ZGEMV" maxdiff))

;;;;
;;;; _GER_
;;;;

(defun ger (alpha x incx y incy a &optional conj)
  (when (/= (abs incx) 1) (error "increment has to be +1 or -1"))
  (when (/= (abs incy) 1) (error "increment has to be +1 or -1"))
  (let ((x (subseq x 0 (array-dimension a 1)))
	(y (subseq y 0 (array-dimension a 0))))
    (when (minusp incx) (setf x (reverse x)))
    (when (minusp incy) (setf y (reverse y)))
    (+ (* alpha
	  (outer-product (if conj (conjugate y) y) x))
       a)))

(let ((a (coerce da '(vector c-double)))
      (x (coerce dx '(vector c-double)))
      (y (coerce dy '(vector c-double)))
      (val nil)
      (alpha 2)
      (maxdiff 0))
  (dolist (incx '(1 -1))
    (dolist (incy '(1 -1))
      (setf val (copy-seq a))
      (blas-dger 3 2 alpha x 0 incx y 0 incy val 0 3)
      (let ((v (ger alpha x incx y incy (matrix '(2 3) a))))
	(when *verbose-tests*
	      (format t "DGER ~2d ~2d: ~f~%"
		      incx
		      incy
		      (max (abs (- val v)))))
	(setf maxdiff (max maxdiff (max (abs (- val v))))))))
  (test-report "DGER" maxdiff))

(let ((a (coerce (complex da (* 2 da)) '(vector c-dcomplex)))
      (x (coerce (complex dx (* 3 dx)) '(vector c-dcomplex)))
      (y (coerce (complex dy (* 4 dy)) '(vector c-dcomplex)))
      (val nil)
      (alpha (complex 2 4))
      (maxdiff 0))
  (dolist (incx '(1 -1))
    (dolist (incy '(1 -1))
      (setf val (copy-seq a))
      (blas-zgerc 3 2 alpha x 0 incx y 0 incy val 0 3)
      (let ((v (ger alpha x incx y incy (matrix '(2 3) a) t)))
	(when *verbose-tests*
	      (format t "ZGERC ~2d ~2d: ~f~%"
		      incx
		      incy
		      (max (abs (- val v)))))
	(setf maxdiff (max maxdiff (max (abs (- val v))))))))
  (test-report "ZGERC" maxdiff))

(let ((a (coerce (complex da (* 2 da)) '(vector c-dcomplex)))
      (x (coerce (complex dx (* 3 dx)) '(vector c-dcomplex)))
      (y (coerce (complex dy (* 4 dy)) '(vector c-dcomplex)))
      (val nil)
      (alpha (complex 2 4))
      (maxdiff 0))
  (dolist (incx '(1 -1))
    (dolist (incy '(1 -1))
      (setf val (copy-seq a))
      (blas-zgeru 3 2 alpha x 0 incx y 0 incy val 0 3)
      (let ((v (ger alpha x incx y incy (matrix '(2 3) a))))
	(when *verbose-tests*
	      (format t "ZGERU ~2d ~2d: ~f~%"
		      incx
		      incy
		      (max (abs (- val v)))))
	(setf maxdiff (max maxdiff (max (abs (- val v))))))))
  (test-report "ZGERU" maxdiff))


;;;;
;;;; _TRMV
;;;;

(defun trmv (a x &optional upper transpose unit incx)
  (when (/= (abs incx) 1) (error "increment has to be +1 or -1"))
  (cond
   ((equalp transpose "n")
    (setf a (transpose a)))
   (t (setf upper (if (equalp upper "u") "l" "u"))))
  (when (equalp transpose "c") (setf a (conjugate a)))
  (when (minusp incx) (setf x (reverse x)))
  (let* ((n (array-dimension a 0))
	 (y (make-array n :initial-element 0)))
    (when (not (equalp unit "n"))
	  (setf a (copy-array a))
	  (dotimes (i n) (setf (aref a i i) 1)))
    (if (equalp upper "u")
	(dotimes (i n)
          (do ((j i (+ j 1)))
	      ((>= j n))
	      (incf (aref y i) (* (aref a i j) (aref x j)))))
        (dotimes (i n)
	  (dotimes (j (+ i 1))
	    (incf (aref y i) (* (aref a i j) (aref x j))))))
    (if (minusp incx) (reverse y) y)))

(let ((b (coerce db '(vector c-double)))
      (x (coerce dx '(vector c-double)))
      (val nil)
      (maxdiff 0))
  (dolist (upper '("u" "l"))
    (dolist (trans '("n" "t"))
      (dolist (diag '("n" "u"))
	(dolist (incx '(1 -1))
	  (setf val (copy-seq x))
	  (blas-dtrmv upper trans diag 3 b 0 3 val 0 incx)
	  (let ((v (trmv (matrix '(3 3) b) x upper trans diag incx)))
	    (when *verbose-tests*
		  (format t "DTRMV ~a ~a ~a ~2d: ~f~%"
			  upper
			  trans
			  diag
			  incx
			  (max (abs (- val v)))))
	    (setf maxdiff (max maxdiff (max (abs (- val v))))))))))
  (test-report "DTRMV" maxdiff))

(let ((b (coerce (complex db (* 2 db)) '(vector c-dcomplex)))
      (x (coerce (complex dx (* 3 dx)) '(vector c-dcomplex)))
      (val nil)
      (maxdiff 0))
  (dolist (upper '("u" "l"))
    (dolist (trans '("n" "t" "c"))
      (dolist (diag '("n" "u"))
	(dolist (incx '(1 -1))
	  (setf val (copy-seq x))
	  (blas-ztrmv upper trans diag 3 b 0 3 val 0 incx)
	  (let ((v (trmv (matrix '(3 3) b) x upper trans diag incx)))
	    (when *verbose-tests*
		  (format t "ZTRMV ~a ~a ~a ~2d: ~f~%"
			  upper
			  trans
			  diag
			  incx
			  (max (abs (- val v)))))
	    (setf maxdiff (max maxdiff (max (abs (- val v))))))))))
  (test-report "ZTRMV" maxdiff))

(defun trsv (a x &optional upper transpose unit incx)
  (when (/= (abs incx) 1) (error "increment has to be +1 or -1"))
  (cond
   ((equalp transpose "n")
    (setf a (transpose a)))
   (t (setf upper (if (equalp upper "u") "l" "u"))))
  (when (equalp transpose "c") (setf a (conjugate a)))
  (when (minusp incx) (setf x (reverse x)))
  (let* ((n (array-dimension a 0))
	 (y (make-array n :initial-contents x)))
    (if (equalp upper "u")
	(do ((i (- n 1) (- i 1)))
	    ((< i 0))
	    (do ((j (+ i 1) (+ j 1)))
		((>= j n))
		(decf (aref y i) (* (aref a i j) (aref y j))))
	    (when (equalp unit "n")
		  (setf (aref y i) (/ (aref y i) (aref a i i)))))
        (dotimes (i n)
	  (dotimes (j i)
	    (decf (aref y i) (* (aref a i j) (aref y j))))
	  (when (equalp unit "n")
		(setf (aref y i) (/ (aref y i) (aref a i i))))))
    (if (minusp incx) (reverse y) y)))

(let ((b (coerce db '(vector c-double)))
      (x (coerce dx '(vector c-double)))
      (val nil)
      (maxdiff 0))
  (dolist (upper '("u" "l"))
    (dolist (trans '("n" "t"))
      (dolist (diag '("n" "u"))
	(dolist (incx '(1 -1))
	  (setf val (copy-seq x))
	  (blas-dtrsv upper trans diag 3 b 0 3 val 0 incx)
	  (let ((v (trsv (matrix '(3 3) b) x upper trans diag incx)))
	    (when *verbose-tests*
		  (format t "DTRSV ~a ~a ~a ~2d: ~f~%"
			  upper
			  trans
			  diag
			  incx
			  (max (abs (- val v)))))
	    (setf maxdiff (max maxdiff (max (abs (- val v))))))))))
  (test-report "DTRSV" maxdiff))

(let ((b (coerce (complex db (* 2 db)) '(vector c-dcomplex)))
      (x (coerce (complex dx (* 3 dx)) '(vector c-dcomplex)))
      (val nil)
      (maxdiff 0))
  (dolist (upper '("u" "l"))
    (dolist (trans '("n" "t"))
      (dolist (diag '("n" "u"))
	(dolist (incx '(1 -1))
	  (setf val (copy-seq x))
	  (blas-ztrsv upper trans diag 3 b 0 3 val 0 incx)
	  (let ((v (trsv (matrix '(3 3) b) x upper trans diag incx)))
	    (when *verbose-tests*
		  (format t "ZTRSV ~a ~a ~a ~2d: ~f~%"
			  upper
			  trans
			  diag
			  incx
			  (max (abs (- val v)))))
	    (setf maxdiff (max maxdiff (max (abs (- val v))))))))))
  (test-report "ZTRSV" maxdiff))



;;;;
;;;; _GEMM
;;;;

(defun gemm (transa transb alpha a b beta c)
  (when (equalp transa "n") (setf a (transpose a)))
  (when (equalp transa "c") (setf a (conjugate a)))
  (when (equalp transb "n") (setf b (transpose b)))
  (when (equalp transb "c") (setf b (conjugate b)))
  (let* ((m (array-dimension c 1))
	 (n (array-dimension c 0))
	 (k (array-dimension a 1))
	 (z (* beta c)))
    (dotimes (i m)
      (dotimes (j n)
        (dotimes (l k)
	  (incf (aref z j i) (* alpha (aref a i l) (aref b l j))))))
    z))

  
(setf aa (transpose (matrix '(4 2) (iseq 1 8))))
(setf bb (transpose (matrix '(2 3) (iseq 9 14))))
(setf cc (transpose (matrix '(4 3) (iseq 15 26))))

(let ((a (coerce aa '(array c-double)))
      (b (coerce bb '(array c-double)))
      (c (coerce cc '(array c-double)))
      (m 4)
      (n 3)
      (k 2)
      (alpha 2)
      (beta 3)
      (val nil)
      (maxdiff 0))
  (dolist (transa '("n" "t"))
    (dolist (transb '("n" "t"))
      (let ((aa (if (equalp transa "n") a (transpose a)))
	    (bb (if (equalp transb "n") b (transpose b)))
	    (lda (if (equalp transa "n") m k))
	    (ldb (if (equalp transb "n") k n)))
	(setf val (copy-array c))
	(blas-dgemm transa transb m n k alpha aa 0 lda bb 0 ldb beta val 0 m)
	(let ((v (gemm transa transb alpha aa bb beta c)))
	  (when *verbose-tests*
		(format t "DGEMM ~a ~a: ~f~%"
			transa
			transb
			(max (abs (- val v)))))
	  (setf maxdiff (max maxdiff (max (abs (- val v)))))))))
  (test-report "DGEMM" maxdiff))

(let ((a (coerce (complex aa (* 2 aa)) '(array c-dcomplex)))
      (b (coerce (complex bb (* 3 bb)) '(array c-dcomplex)))
      (c (coerce (complex cc (* 4 cc)) '(array c-dcomplex)))
      (m 4)
      (n 3)
      (k 2)
      (alpha #c(2 4))
      (beta #c(3 6))
      (val nil)
      (maxdiff 0))
  (dolist (transa '("n" "t" "c"))
    (dolist (transb '("n" "t" "c"))
      (let ((aa (if (equalp transa "n") a (transpose a)))
	    (bb (if (equalp transb "n") b (transpose b)))
	    (lda (if (equalp transa "n") m k))
	    (ldb (if (equalp transb "n") k n)))
	(setf val (copy-array c))
	(blas-zgemm transa transb m n k alpha aa 0 lda bb 0 ldb beta val 0 m)
	(let ((v (gemm transa transb alpha aa bb beta c)))
	  (when *verbose-tests*
		(format t "ZGEMM ~a ~a: ~f~%"
			transa
			transb
			(max (abs (- val v)))))
	  (setf maxdiff (max maxdiff (max (abs (- val v)))))))))
  (test-report "ZGEMM" maxdiff))


;;;;
;;;; TRSM
;;;;

(defun trsm (side upper transpose unit a alpha x)
  (cond
   ((equalp side "l")
    (let ((cx (* alpha (row-list x))))
      (apply #'bind-rows
	     (mapcar #'(lambda (c) (trsv a c upper transpose unit 1))
		     cx))))
   (t
    (let ((cx (* alpha (column-list x)))
	  (conj (equalp transpose "c")))
      (setf transpose (if (equalp transpose "n") "t" "n"))
      (when conj (setf cx (conjugate cx)))
      (apply #'bind-columns
	     (mapcar #'(lambda (c)
			 (let ((v (trsv a c upper transpose unit 1)))
			   (if conj (conjugate v) v)))
		     cx))))))

(let ((b (coerce db '(vector c-double)))
      (c (coerce dc '(vector c-double)))
      (alpha 2)
      (val nil)
      (maxdiff 0.0))
  (dolist (side '("l" "r"))
    (dolist (uplo '("u" "l"))
      (dolist (trans '("n" "t"))
        (dolist (diag '("n" "u"))
          (setf val (copy-array c))
	  (if (equalp side "l")
	      (blas-dtrsm side uplo trans diag 3 2 alpha b 0 3 val 0 3)
	      (blas-dtrsm side uplo trans diag 2 3 alpha b 0 3 val 0 2))
	  (let* ((mb (matrix '(3 3) b))
		 (mc (matrix (if (equalp side "l") '(2 3) '(3 2)) c))
		 (v (trsm side uplo trans diag mb alpha mc)))
	    (when *verbose-tests*
		  (format t "DTRSM ~a ~a ~a ~a: ~f~%"
			  side
			  uplo
			  trans
			  diag
			  (max (abs (- val v)))))
	    (setf maxdiff (max maxdiff (max (abs (- val v))))))))))
  (test-report "DTRSM" maxdiff))

(let ((b (coerce (complex db (* 2 db)) '(vector c-dcomplex)))
      (c (coerce (complex dc (* 3 dc)) '(vector c-dcomplex)))
      (alpha (complex 2 4))
      (val nil)
      (maxdiff 0))
  (dolist (side '("l" "r"))
    (dolist (uplo '("u" "l"))
      (dolist (trans '("n" "t" "c"))
        (dolist (diag '("n" "u"))
          (setf val (copy-array c))
	  (if (equalp side "l")
	      (blas-ztrsm side uplo trans diag 3 2 alpha b 0 3 val 0 3)
	      (blas-ztrsm side uplo trans diag 2 3 alpha b 0 3 val 0 2))
	  (let* ((mb (matrix '(3 3) b))
		 (mc (matrix (if (equalp side "l") '(2 3) '(3 2)) c))
		 (v (trsm side uplo trans diag mb alpha mc)))
	    (when *verbose-tests*
		  (format t "ZTRSM ~a ~a ~a ~a: ~f~%"
			  side
			  uplo
			  trans
			  diag
			  (max (abs (- val v)))))
	    (setf maxdiff (max maxdiff (max (abs (- val v))))))))))
  (test-report "ZTRSM" maxdiff))


(defun nax+y (a x y)
  (let* ((m (array-dimension a 0))
	 (n (array-dimension a 1))
	 (da (coerce a '(array c-double)))
	 (dx (coerce x '(vector c-double)))
	 (dy (make-array m :element-type 'c-double :initial-contents y)))
    (blas-dgemv "t" n m 1 da 0 n dx 0 1 1 dy 0 1)
    (coerce dy (if (vectorp x) '(vector t) 'list))))
