;;;;
;;;; statistics.lsp XLISP-STAT statistics functions
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;

(provide "stats")

; setf method for select function
(defsetf select set-select)

;;;;
;;;; Data File Reading 
;;;;

(defun count-file-columns (fname)
"Args: (fname)
Returns the number of lisp items on the first nonblank line of file FNAME."
  (with-open-file (f fname)
    (if f
        (let ((line (do ((line (read-line f) (read-line f))) 
                        ((or (null line) (< 0 (length line))) line))))
          (if line
              (with-input-from-string (s line)
                (do ((n 0 (+ n 1)) (eof (gensym))) 
                    ((eq eof (read s nil eof)) n))))))))

(defvar *xlisptable* *readtable*)

(unless (fboundp 'open-file-dialog)
	(defun open-file-dialog (&optional set)
	  (declare (ignore set))
	  (if (system-has-windows)
	      (get-string-dialog "Enter a data file name:")
	      (error "You must provide a file name explicitly"))))

(defun read-data-file (&optional (file (open-file-dialog t)))
"Args:  (file)
Returns a list of all lisp objects in FILE. FILE can be a string or a symbol,
in which case the symbol'f print name is used."
  (if file
      (let ((oldtable *readtable*)
            (oldbreak *breakenable*)
            (eof (gensym)))
        (setq *readtable* *xlisptable*)
        (setq *breakenable* nil)
        (with-open-file (f file)
          (if f
              (unwind-protect
               (do* ((r (read f nil eof) (read f nil eof))
                     (x (list nil))
                     (tail x (cdr tail)))
                    ((eq r eof) (cdr x))
                    (setf (cdr tail) (list r)))
               (setq *breakenable* oldbreak)
               (setq *readtable* oldtable)))))))

;;; New definition to avoid stack size limit in apply
(defun read-data-columns (&optional (file (open-file-dialog t))
                                    (cols (if file 
                                              (count-file-columns file))))
"Args: (&optional file cols)
Reads the data in FILE as COLS columns and returns a list of lists representing the columns."
  (if (and file cols)
      (transpose (split-list (read-data-file file) cols))))

(defun load-data (file)
"Args: (file)
Read in data file from the data examples library."
  (load (merge-pathnames file
			 (make-pathname :directory '(:relative "Data")))))

(defun load-example (file)
"Args: (file)
Read in lisp example file from the examples library."
  (load (merge-pathnames file
			 (make-pathname :directory '(:relative "Examples")))))


;;;;
;;;; Listing and Saving Variables and Functions
;;;;

(defvar *variables* nil)
(defvar *ask-on-redefine* nil)

(defmacro def (symbol value)
"Syntax: (def var form)
VAR is not evaluated and must be a symbol.  Assigns the value of FORM to
VAR and adds VAR to the list *VARIABLES* of def'ed variables. Returns VAR.
If VAR is already bound and the global variable *ASK-ON-REDEFINE*
is not nil then you are asked if you want to redefine the variable."
  `(unless (and *ask-on-redefine*
                (boundp ',symbol)
                (not (y-or-n-p "Variable has a value. Redefine?")))
           (pushnew ',symbol *variables*)
           (setf ,symbol ,value)
           ',symbol))
  
(defun variables ()
"Args:()
Returns a list of the names of all def'ed variables."
  (sort (copy-list *variables*) #'string<=))

;;**** modify to use with-open-file
(defun savevar (vars file)
"Args: (vars file-name-root)
VARS is a symbol or a list of symbols. FILE-NAME-ROOT is a string (or a symbol
whose print name is used) not endinf in .lsp. The VARS and their current values
are written to the file FILE-NAME-ROOT.lsp in a form suitable for use with the
load command."
  (let ((f (open (concatenate 'string (string file) ".lsp")
		 :direction :output))
        (vars (if (consp vars) vars (list vars)))
        (oldbreak *breakenable*))
    (setq *breakenable* nil)
    (unwind-protect
      (mapcar
        (lambda (x)
            (if (objectp (symbol-value x))
                (print `(def ,x ,(send (symbol-value x) :save)) f)
                (print `(def ,x ',(symbol-value x)) f)))
        vars)
      (setq *breakenable* oldbreak)
      (close f))
    vars))

(defun undef (v)
"Args: (v)
If V is the symbol of a defined variable the variable it is unbound and
removed from the list of defined variables. If V is a list of variable
names each is unbound and removed. Returns V."
  (dolist (s (if (listp v) v (list v)))
          (when (member s *variables*)
                (setq *variables* (delete s *variables*))
                (makunbound s)))
  v)
        

;;;;
;;;; Basic Summary Statistics
;;;;

(defun standard-deviation (x)
"Args: (x)
Returns the standard deviation of the elements x. Vector reducing."
  (let ((n (count-elements x))
        (r (- x (mean x))))
    (sqrt (* (mean (* r r)) (/ n (- n 1))))))

(defun quantile (x p)
"Args: (x p)
Returns the P-th quantile(s) of sequence X. P can be a number or a sequence."
  (let* ((x (sort-data x))
         (n (length x))
         (np (* p (- n 1)))
         (low (floor np))
         (high (ceiling np)))
    (/ (+ (select x low) (select x high)) 2)))
    
(defun median (x) 
"Args: (x)
Returns the median of the elements of X."
  (quantile x 0.5))

(defun interquartile-range (x) 
"Args: (number-data)
Returns the interquartile range of the elements of X."
  (apply #'- (quantile x '(0.75 0.25))))

(defun fivnum (x) 
"Args: (number-data)
Returns the five number summary (min, 1st quartile, medinan, 3rd quartile,
max) of the elements X."
  (quantile x '(0 .25 .5 .75 1)))

(defun covariance-matrix (&rest args)
"Args: (&rest args)
Returns the sample covariance matrix of the data columns in ARGS. ARGS may
consist of lists, vectors or matrices."
  (let ((columns (apply #'append 
                        (mapcar (lambda (x) 
                                  (if (matrixp x) (column-list x) (list x)))
                                args))))
    (/ (cross-product (apply #'bind-columns 
                             (- columns (mapcar #'mean columns))))
       (- (length (car columns)) 1))))

;;;;
;;;; Basic Sequence Operations
;;;;

(defun difference (x)
"Args: (x)
Returns differences for a sequence X."
  (let ((n (length x)))
    (- (select x (iseq 1 (1- n))) (select x (iseq 0 (- n 2))))))

(defun rseq (a b num)
"Args: (a b num)
Returns a list of NUM equally spaced points starting at A and ending at B."
  (+ a (* (iseq 0 (1- num)) (/ (- b a) (1- num)))))


;;;;
;;;; Linear Algebra Functions
;;;;

(defun matrix (dim data)
"Args: (dim data)
returns a matrix of dimensions DIM initialized using sequence DATA
in row major order." 
  (let ((dim (coerce dim 'list))
        (data (coerce data 'list)))
    (make-array dim :initial-contents (split-list data (nth 1 dim)))))

#|
(defun print-matrix (a &optional (stream *standard-output*))
"Args: (matrix &optional stream)
Prints MATRIX to STREAM in a nice form that is still machine readable"
  (unless (matrixp a) (error "not a matrix - ~a" a))
  (let ((size (min 15 (max (map-elements #'flatsize a)))))
    (format stream "#2a(~%")
    (dolist (x (row-list a))
            (format stream "    (")
            (let ((n (length x)))
              (dotimes (i n)
                       (let ((y (aref x i)))
                         (cond
                           ((integerp y) (format stream "~vd" size y))
                           ((floatp y) (format stream "~vg" size y))
                           (t (format stream "~va" size y))))
                       (if (< i (- n 1)) (format stream " "))))
            (format stream ")~%"))
    (format stream "   )~%")
    nil))
|#
;; **** temporary modification for new printing -- needs rethinking
(defun print-matrix (a &optional (stream *standard-output*)
		       &key (float-digits 6))
"Args: (matrix &optional stream &key (float-digits 6))
Prints MATRIX to STREAM in a nice form that is still machine readable"
  (unless (matrixp a) (error "not a matrix - ~a" a))
  (let ((float-size (+ 7 float-digits))
	(size 0))
    (map-elements
     #'(lambda (x)
	 (setf size
	       (max size
		    (cond
		     ((floatp x) float-size)
		     ((integerp x) (+ (flatsize x) 4))
		     (t (flatsize x))))))
     a)
    (format stream "#2a(~%")
    (dolist (x (row-list a))
      (format stream "    (")
      (let ((n (length x)))
	(dotimes (i n)
	  (let ((y (aref x i)))
	    (cond
	     ((integerp y) (format stream "~vd~4@t" (- size 4) y))
	     ((floatp y) (format stream "~v,vg" size float-digits y))
	     (t (format stream "~va" size y))))
	  (if (< i (- n 1)) (format stream " "))))
      (format stream ")~%"))
    (format stream "   )~%")
    nil))

(defun array-to-nested-list (array)
  (let ((n (array-rank array))
	(alist (combine array)))
    (do ((i (- n 1) (- i 1)))
	((<= i 0) alist)
	(setf alist (split-list alist (array-dimension array i))))))

(defun solve (a b)
"Args: (a b)
Solves A x = B using LU decomposition and backsolving. B can be a sequence
or a matrix."
  (let ((lu (lu-decomp a)))
    (if (matrixp b)
        (apply #'bind-columns 
               (mapcar #'(lambda (x) (lu-solve lu x)) (column-list b)))
        (lu-solve lu b))))
        
(defun backsolve (a b)
"Args: (a b)
Solves A x = B by backsolving, assuming A is upper triangular. B must be a
sequence. For use with qr-decomp."
  (let* ((n (length b))
         (sol (make-array n)))
    (dotimes (i n)
             (let* ((k (- n i 1))
                    (val (elt b k)))
               (dotimes (j i)
                        (let ((l (- n j 1)))
                          (setq val (- val (* (aref sol l) (aref a k l))))))
               (setf (aref sol k) (/ val (aref a k k)))))
    (if (listp b) (coerce sol 'list) sol)))

(defun eigenvalues (a) 
"Args: (a)
Returns list of eigenvalues of square, symmetric matrix A"
  (first (eigen a)))

(defun eigenvectors (a) 
"Args: (a)
Returns list of eigenvectors of square, symmetric matrix A"
  (second (eigen a)))

(defun accumulate (f s)
"Args: (f s)
Accumulates elements of sequence S using binary function F.
(accumulate #'+ x) returns the cumulative sum of x."
  (let* ((result (list (elt s 0)))
         (tail result))
    (flet ((acc (dummy x)
                (declare (ignore dummy))
                (rplacd tail (list (funcall f (first tail) x)))
                (setf tail (cdr tail))))
      (reduce #'acc s))
    (if (vectorp s) (coerce result 'vector) result)))

(defun cumsum (x)
"Args: (x)
Returns the cumulative sum of X."
  (accumulate #'+ x))

(defun combine (&rest args) 
"Args (&rest args) 
Returns sequence of elements of all arguments."
  (copy-seq (element-seq args)))

(defun lowess (x y &key (f .25) (steps 2) (delta -1) sorted)
"Args: (x y &key (f .25) (steps 2) delta sorted)
Returns (list X YS) with YS the LOWESS fit. F is the fraction of data used for
each point, STEPS is the number of robust iterations. Fits for points within
DELTA of each other are interpolated linearly. If the X values setting SORTED
to T speeds up the computation."
  (multiple-value-bind
   (x y)
   (let ((x (coerce x '(vector c-double)))
	 (y (coerce y '(vector c-double))))
     (if sorted
	 (values x y)
	 (let ((ord (order x)))
	   (values (select x ord) (select y ord)))))
   (let* ((n (length x))
	  (ys (make-array n :element-type 'c-double))
	  (rw (make-array n :element-type 'c-double))
	  (res (make-array n :element-type 'c-double))
	  (delta (if (> delta 0.0) delta (/ (- (max x) (min x)) 50))))
     (when (base-lowess x y n f steps delta ys rw res)
	   (error "bad lowess data"))
     (list (coerce x 'list) (coerce ys 'list)))))

(defparameter *default-smoother-points* 30)

(defun spline (x y &key (xvals *default-smoother-points*))
"Args: (x y &key xvals)
Returns list of x and y values of natural cubic spline interpolation of (X,Y).
X must be strictly increasing. XVALS can be an integer, the number of equally
spaced points to use in the range of X, or it can be a sequence of points at 
which to interpolate."
  (multiple-value-bind
   (n x y ns xs ys)
   (get-smoother-data x y xvals t)
   (let ((work (make-array (* 2 n) :element-type 'c-double)))
     (when (base-spline n x y ns xs ys work) (error "bad spline data"))
     (list (coerce xs 'list) (coerce ys 'list)))))

(defun kernel-dens (x &key
		      (xvals *default-smoother-points*)
		      (width -1.0)
		      (type 'B))
"Args: (x &key xvals width type)
Returns list of x and y values of kernel density estimate of X. XVALS can be an
integer, the number of equally spaced points to use in the range of X, or it
can be a sequence of points at which to interpolate. WIDTH specifies the
window width. TYPE specifies the lernel and should be one of the symbols G, T,
U or B for gaussian, triangular, uniform or bisquare. The default is B."
  (multiple-value-bind
   (n x y ns xs ys)
   (get-smoother-data x nil xvals nil)
   (when (base-kernel-smooth n x y ns xs ys width type)
	 (error "bad data for smoother"))
   (list (coerce xs 'list) (coerce ys 'list))))

(defun kernel-smooth (x y &key
			(xvals *default-smoother-points*)
			(width -1.0)
			(type 'B))
"Args: (x y &key xvals width type)
Returns list of x and y values of kernel smooth of (X,Y). XVALS can be an
integer, the number of equally spaced points to use in the range of X, or it
can be a sequence of points at which to interpolate. WIDTH specifies the
window width. TYPE specifies the lernel and should be one of the symbols G, T,
U or B for Gaussian, triangular, uniform or bisquare. The default is B."
  (multiple-value-bind
   (n x y ns xs ys)
   (get-smoother-data x y xvals t)
   (when (base-kernel-smooth n x y ns xs ys width type)
	 (error "bad data for smoother"))
   (list (coerce xs 'list) (coerce ys 'list))))

;;;;
;;;; Sorting Functions
;;;; (Moved to Lisp since MWERKS qsort is awful)
;;;;

(defun sort-data (x)
  (let ((xlist (if (listp x)
		   (copy-list x)
		 (coerce (compound-data-seq x) 'list))))
    (sort xlist #'sort-data<)))

(defun order (x)
  (let* ((xlist (if (listp x) x (coerce (compound-data-seq x) 'list)))
	 (data (mapcar #'cons xlist (iseq (length xlist))))
	 (sdata (sort data #'order<)))
    (mapcar #'cdr sdata)))

(defun xlisp::make-compound (form seq)
  (cond
   ((listp form) (coerce seq 'list))
   ((stringp form) (error "not a combound data item - ~s" seq))
   ;;**** preserve element type??
   ((vectorp form) (coerce seq 'vector))
   ((arrayp form)
    (make-array (array-dimensions form) :displaced-to (coerce seq 'vector)))
   (t (error "not a combound data item - ~s" seq))))

(defun rank (x) (xlisp::make-compound x (order (order x))))
