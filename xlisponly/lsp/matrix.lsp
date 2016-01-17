; Matrix functions by Tom Almy
; Multidimensional arrays are implemented here as arrays of arrays
; make-array is redefined to mimic common lisp
; Unfortunately AREF cannot be changed since its operation in setf is
; "wired in", so we will use a new (macro) function MREF


(when (eq (type-of (symbol-function 'make-array))
	  'subr)
      (setf (symbol-function 'orig-make-array)
	    (symbol-function 'make-array)))

(defun make-array (dims &key initial)
    (cond ((null dims) initial)
	  ((atom dims) (make-array (list dims) :initial initial))
	  (t (let ((result (orig-make-array (first dims))))
	       (when (or (rest dims) initial)
		     (dotimes (i (first dims))
			      (setf (aref result i)
				    (make-array (rest dims) :initial initial))))
	       result))))

(defun mref (matrix &rest indices)
    (dolist (index indices)
	    (setq matrix (aref matrix index)))
    matrix)

(setf (get 'mref '*setf*)
      #'(lambda (mat &rest arglist)
	  (do ((index (first arglist) (first remainder))
	       (remainder (rest arglist) (rest remainder)))
	      ((null (rest remainder))
	       (setf (aref mat index) (first remainder)))
	    (setf mat (aref mat index)))))

