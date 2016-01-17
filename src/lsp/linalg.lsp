;;;;
;;;; linalg.lsp XLISP-STAT linear algebra functions
;;;; XLISP-STAT 2.1 Copyright (c) 1990-1995, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;

(in-package "XLISP")

(provide "linalg")


;;;;
;;;; Basic Matrix Operations
;;;;

(export '(matmult %*
	  inner-product cross-product outer-product
	  identity-matrix))

(defun multiply-matrix-matrix (x y)
  (multiple-value-bind
   (fun type atype)
   (if (any-complex-elements x y)
       (values #'blas-zgemm 'c-dcomplex '(array c-dcomplex))
       (values #'blas-dgemm 'c-double '(array c-double)))
   (let* ((x (coerce x atype))
	  (y (coerce y atype))
	  (m (array-dimension x 0))
	  (k (array-dimension x 1))
	  (ky (array-dimension y 0))
	  (n (array-dimension y 1))
	  (v (make-array (list m n) :element-type type)))
     (unless (= ky k) (error "dimensions do not match"))
     (funcall fun "n" "n" n m k 1 y 0 n x 0 k 0 v 0 n)
     (coerce v '(array t)))))

(defun multiply-matrix-vector (x y)
  (multiple-value-bind
   (fun type atype)
   (if (any-complex-elements x y)
       (values #'blas-zgemv 'c-dcomplex '(array c-dcomplex))
       (values #'blas-dgemv 'c-double '(array c-double)))
   (let* ((vtype (if (listp y) 'list '(array t)))
	  (x (coerce x atype))
	  (y (coerce y atype))
	  (m (array-dimension x 0))
	  (k (array-dimension x 1))
	  (ky (length y))
	  (v (make-array m :element-type type)))
     (unless (= ky k) (error "dimensions do not match"))
     (funcall fun "t" k m 1 x 0 k y 0 1 0 v 0 1)
     (coerce v vtype))))

(defun multiply-vector-matrix (x y)
  (multiple-value-bind
   (fun type atype)
   (if (any-complex-elements x y)
       (values #'blas-zgemv 'c-dcomplex '(array c-dcomplex))
       (values #'blas-dgemv 'c-double '(array c-double)))
   (let* ((vtype (if (listp x) 'list '(array t)))
	  (x (coerce x atype))
	  (y (coerce y atype))
	  (k (length x))
	  (ky (array-dimension y 0))
	  (n (array-dimension y 1))
	  (v (make-array n :element-type type)))
     (unless (= ky k) (error "dimensions do not match"))
     (funcall fun "n" n k 1 y 0 n x 0 1 0 v 0 1)
     (coerce v vtype))))

(defun inner-product (x y &optional (conjugate t))
  (multiple-value-bind
   (fun atype)
   (if (any-complex-elements x y)
       (values (if conjugate #'blas-zdotc #'blas-zdotu) '(array c-dcomplex))
       (values #'blas-ddot '(array c-double)))
   (let* ((x (coerce x atype))
	  (y (coerce y atype))
	  (k (length x))
	  (ky (length y)))
     (unless (= ky k) (error "dimensions do not match"))
     (funcall fun k y 0 1 x 0 1))))

(defun binary-matmult (x y)
  (cond
   ((matrixp x)
    (cond
     ((matrixp y) (multiply-matrix-matrix x y))
     ((sequencep y) (multiply-matrix-vector x y))
     (t (* x y))))
   ((sequencep x)
    (cond
     ((matrixp y) (multiply-vector-matrix x y))
     ((sequencep y) (inner-product x y nil))
     (t (* x y))))
   (t (* x y))))

(defun matmult (x &rest more)
  (reduce #'binary-matmult more :initial-value x))

(setf (symbol-function '%*) #'matmult)


(defun cross-product (x &optional (conjugate t))
  (if (sequencep x)
      (inner-product x x conjugate)
      (multiple-value-bind
       (fun type atype trans)
       (if (any-complex-elements x)
	   (values #'blas-zgemm
		   'c-dcomplex
		   '(array c-dcomplex)
		   (if conjugate "c" "t"))
	   (values #'blas-dgemm 'c-double '(array c-double) "t"))
       (let* ((x (coerce x atype))
	      (m (array-dimension x 0))
	      (n (array-dimension x 1))
	      (v (make-array (list n n) :element-type type)))
	 (funcall fun "n" trans n n m 1 x 0 n x 0 n 0 v 0 n)
	 (coerce v '(array t))))))

(defun outer-product (x y &optional f)
  (unless (compound-data-p x) (setf x (vector x)))
  (unless (compound-data-p y) (setf y (vector y)))
  (let* ((x (coerce (compound-data-seq x) 'vector))
	 (y (coerce (compound-data-seq y) 'vector))
	 (m (length x))
	 (n (length y))
	 (v (make-array (list m n))))
    (if f 
	(dotimes (i m)
	  (dotimes (j n)
	    (setf (aref v i j) (funcall f (aref x i) (aref y j)))))
        (dotimes (i m)
	  (dotimes (j n)
	    (setf (aref v i j) (* (aref x i) (aref y j))))))
    v))

(defun identity-matrix (n)
  (diagonal (make-list n :initial-element 1)))


;;;;
;;;; TRANSPOSE
;;;;

(export 'transpose)

(defun transpose (x)
  (cond
   ((matrixp x) (permute-array x '(1 0)))
   ((consp x) (transpose-list x))
   (t (error "bad argumant type - ~s" x))))


;;;;
;;;; SWEEP Operator
;;;; 

(export '(make-sweep-matrix sweep-operator))

(defun make-sweep-matrix (x y &optional w)
  (let* ((n (array-dimension x 0))
	 (p (array-dimension x 1))
	 (x (coerce x '(array c-double)))
	 (y (coerce y '(vector c-double)))
	 (w (if w
		(coerce w '(vector c-double))
	        (make-array n :element-type 'c-double :initial-element 1.0)))
	 (sm (make-array (list (+ p 2) (+ p 2)) :element-type 'c-double))
	 (xmean (make-array p :element-type 'c-double)))
    (base-make-sweep-matrix n p x y w sm xmean)
    (coerce sm '(array t))))

(defun sweep-operator (a cols &optional (tol .000001))
  (let* ((m (array-dimension a 0))
	 (n (array-dimension a 1))
	 (a (make-array (* m n)
			:element-type 'c-double
			:initial-contents (compound-data-seq a)))
	 (tols (if (numberp tol) (repeat tol (length cols)) tol))
	 (swept nil))
    (loop
     (if (or (null cols) (null tols)) (return))
     (let ((k (pop cols))
	   (tol (pop tols)))
       (if (sweep-in-place m n a k tol) (push k swept))))
    (list (make-array (list m n) :displaced-to (coerce a '(array t))) swept)))


;;;;
;;;; Utilities for LINPACK Interface
;;;;

#|
(defun generic-to-linalg (x m n type &optional trans)
  (if trans
      (let ((xv (make-array (* m n) :element-type type)))
	(transpose-into x m n xv)
	xv)
      (make-array (* m n)
		  :element-type type
		  :initial-contents (compound-data-seq x))))

(defun linalg-to-generic (x dim &optional trans)
  (let ((val (make-array dim)))
    (if trans
	(transpose-into x (second dim) (first dim) val)
        (replace (compound-data-seq val) x))
    val))
|#

(defun square-matrix-p (x)
  (and (matrixp x) (= (array-dimension x 0) (array-dimension x 1)))) 

(defmacro check-square-matrix (x)
 `(unless (square-matrix-p ,x) (error "not a square matrix -- ~s" ,x)))

(defmacro check-matrix (x)
  `(unless (matrixp ,x) (error "not a matrix -- ~s" ,x)))


;;;;
;;;; LU Decomposition, Determinant, and Inverse
;;;;

(export '(lu-decomp rcondest determinant inverse lu-solve))

(defun lu-decomp (x)
  (check-square-matrix x)
  (multiple-value-bind
   (fun type)
   (if (any-complex-elements x)
       (values #'linpack-zgefa 'c-dcomplex)
       (values #'linpack-dgefa 'c-double))
   (let* ((n (array-dimension x 0))
	  (xv (generic-to-linalg x n n type t))
	  (ipvt (make-array n :element-type 'c-int))
	  (info (funcall fun xv 0 n n ipvt))
	  (odd nil)
	  (im1 (1- ipvt)))
     (dotimes (i n) (unless (= i (aref im1 i)) (setf odd (not odd))))
     (list (linalg-to-generic xv (list n n) t)
	   im1
	   (if odd -1.0 1.0)
	   (/= info 0.0)))))

(defun rcondest (x)
  (check-square-matrix x)
  (multiple-value-bind
   (fun type)
   (if (any-complex-elements x)
       (values #'linpack-zgeco 'c-dcomplex)
       (values #'linpack-dgeco 'c-double))
   (let* ((n (array-dimension x 0))
	  (xv (generic-to-linalg x n n type t))
	  (ipvt (make-array n :element-type 'c-int))
	  (z (make-array n :element-type type)))
    (funcall fun xv 0 n n ipvt z))))

(defun determinant (x)
  (check-square-matrix x)
  (multiple-value-bind
   (fun1 fun2 type)
   (if (any-complex-elements x)
       (values #'linpack-zgefa #'linpack-zgedi 'c-dcomplex)
       (values #'linpack-dgefa #'linpack-dgedi 'c-double))
   (let* ((n (array-dimension x 0))
	  (xv (generic-to-linalg x n n type t))
	  (ipvt (make-array n :element-type 'c-int))
	  (det (make-array 2 :element-type type))
	  (work (make-array n :element-type type)))
     (funcall fun1 xv 0 n n ipvt)
     (funcall fun2 xv 0 n n ipvt det work 10)
     (* (aref det 0) (^ 10 (aref det 1))))))

(defun inverse (x)
  (check-square-matrix x)
  (multiple-value-bind
   (fun1 fun2 type)
   (if (any-complex-elements x)
       (values #'linpack-zgefa #'linpack-zgedi 'c-dcomplex)
       (values #'linpack-dgefa #'linpack-dgedi 'c-double))
   (let* ((n (array-dimension x 0))
	  (xv (generic-to-linalg x n n type t))
	  (ipvt (make-array n :element-type 'c-int))
	  (work (make-array n :element-type type)))
     (funcall fun1 xv 0 n n ipvt)
     (funcall fun2 xv 0 n n ipvt nil work 1)
     (linalg-to-generic xv (list n n) t))))

(defun lu-solve (lu b)
  (let ((x (first lu))
	(i (+ (second lu) 1)))
    (check-square-matrix x)
    (multiple-value-bind
     (fun type)
     (if (any-complex-elements x b)
	 (values #'linpack-zgesl 'c-dcomplex)
         (values #'linpack-dgesl 'c-double))
     (let* ((n (array-dimension x 0))
	    (xv (generic-to-linalg x n n type t))
	    (ipvt (generic-to-linalg i n 1 'c-int))
	    (bv (generic-to-linalg b n 1 type)))
       (funcall fun xv 0 n n ipvt bv 0)
       (coerce bv (if (vectorp b) '(vector t) 'list))))))


;;;;
;;;; QR and SV Decompositions
;;;;

(export '(qr-decomp sv-decomp))

(defun qr-decomp (x &optional pivot)
  (check-matrix x)
  (multiple-value-bind
   (fun type)
   (if (any-complex-elements x)
       (values #'linpack-zqrdc 'c-dcomplex)
       (values #'linpack-dqrdc 'c-double))
   (let* ((n (array-dimension x 0))
	  (p (array-dimension x 1))
	  (xv (generic-to-linalg x n p type t))
	  (a (make-array p :element-type type))
	  (r (make-array (list p p) :element-type type))
	  (q (make-array (list n p) :element-type type))
	  (j (if pivot (make-array p :element-type 'c-int :initial-element 0)))
	  (w (if pivot (make-array p :element-type type)))
	  (job (if pivot 1 0)))
     (funcall fun xv 0 n n p a j w job r q)
     (let ((gq (coerce q '(array t)))
	   (gr (coerce r '(array t)))
	   (gj (if pivot (coerce (1- j) '(vector t)))))
       (if pivot (list gq gr gj) (list  gq gr))))))

(defun sv-decomp (x)
  (check-matrix x)
  (multiple-value-bind
   (fun type)
   (if (any-complex-elements x)
       (values #'linpack-zsvdc 'c-dcomplex)
       (values #'linpack-dsvdc 'c-double))
   (let ((n (array-dimension x 0))
	 (p (array-dimension x 1)))
     (unless (<= p n) (error "more columns than rows - ~s" x))
     (let* ((xv (generic-to-linalg x n p type t))
	    (s (make-array p :element-type type))
	    (e (make-array p :element-type type))
	    (u (make-array (* n p) :element-type type))
	    (v (make-array (* p p) :element-type type))
	    (work (make-array n :element-type type))
	    (job 21))
       (let ((info (funcall fun xv 0 n n p s e u 0 n v 0 p work job)))
	 (list (linalg-to-generic u (list n p) t)
	       (coerce s '(vector t))
	       (linalg-to-generic v (list p p) t)
	       (if info nil t)))))))


;;;;
;;;; Eigenvalues and Eigenvectors
;;;;

(export 'eigen)

(defun eigen (x)
  (check-square-matrix x)
  (cond
   ((any-complex-elements x)
    (let* ((n (array-dimension x 0))
	   (xr (generic-to-linalg (realpart x) n n 'c-double t))
	   (xi (generic-to-linalg (imagpart x) n n 'c-double t))
	   (w (make-array n :element-type 'c-double))
	   (zr (make-array (list n n) :element-type 'c-double))
	   (zi (make-array (list n n) :element-type 'c-double))
	   (fv1 (make-array n :element-type 'c-double))
	   (fv2 (make-array n :element-type 'c-double))
	   (fm1 (make-array (* 2 n) :element-type 'c-double))
	   (ierr (eispack-ch n n xr xi w 1 zr zi fv1 fv2 fm1)))
      (list (nreverse (coerce w '(vector t)))
	    (nreverse (row-list (coerce (complex zr zi) '(array t))))
	    (if ierr (- n ierr) nil))))
   (t
    (let* ((n (array-dimension x 0))
	   (x (generic-to-linalg x n n 'c-double t))
	   (w (make-array n :element-type 'c-double))
	   (z (make-array (list n n) :element-type 'c-double))
	   (fv1 (make-array n :element-type 'c-double))
	   (fv2 (make-array n :element-type 'c-double))
	   (ierr (eispack-rs n n x w 1 z fv1 fv2)))
      (list (nreverse (coerce w '(vector t)))
	    (nreverse (row-list (coerce z '(array t))))
	    (if ierr (- n ierr) nil))))))
