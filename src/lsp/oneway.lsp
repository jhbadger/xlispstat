;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.

(provide "oneway")

(require "regress")

;;;;
;;;;
;;;; One Way ANOVA Model Prototype
;;;;
;;;;

(defproto oneway-model-proto '(grouped-data) '() regression-model-proto)

(defun oneway-model (data &key (print t) group-names)
"Args: ( data &key (print t))
DATA: list of compound-data
Example:"
  (let ((data (mapcar #'(lambda (x) (coerce x 'list)) data))
        (m (send oneway-model-proto :new)))
    (send m :grouped-data data)
    (send m :group-names group-names)
    (if print (send m :display))
    m))

(defmeth oneway-model-proto :display ()
"Message args: ()
Prints the least squares regression summary."
  (call-next-method)
  (format t "Group Mean Square:~25t~13,6g~40t(~,6g)~%"
          (send self :group-mean-square) (send self :group-df))
  (format t "Error Mean Square:~25t~13,6g~40t(~,6g)~%"
          (send self :error-mean-square) (send self :error-df))
  (format t "~%"))

(defmeth oneway-model-proto  :save ()
"Message args: ()
Returns an expression that will reconstruct the model."
  `(oneway-model ',(send self :grouped-data) 
                 :group-names ',(send self :group-names)))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth oneway-model-proto :grouped-data (&optional data)
"Message args: (&optional data)
Sets or returns the grouped data."
  (when data
        (let* ((y (apply #'append data))
               (indices (repeat (iseq 0 (- (length data) 1)) 
                                (mapcar #'length data)))
               (levels (remove-duplicates indices))
               (indicators (mapcar #'(lambda (x) (if-else (= x indices) 1 0))
                                   levels))
               (x (apply #'bind-columns indicators)))
          (setf (slot-value 'y) y)
          (setf (slot-value 'x) x)
          (setf (slot-value 'intercept) nil)
          (setf (slot-value 'grouped-data) data)
          (send self :needs-computing t)))
   (slot-value 'grouped-data))

(defmeth oneway-model-proto :group-names (&optional (names nil set))
"Method args: (&optional names)
Sets or returns group names."
  (if set (setf (slot-value 'predictor-names) names))
  (let ((g-names (slot-value 'predictor-names))
        (ng (length (slot-value 'grouped-data))))
    (if (not (and g-names (= ng (length g-names))))
        (setf (slot-value 'predictor-names)
              (mapcar #'(lambda (a) (format nil "Group ~a" a)) 
                      (iseq 0 (- ng 1))))))
  (slot-value 'predictor-names))

;;;
;;; Overrides for Linear Regression Methods
;;;

(defmeth oneway-model-proto :y ()
"
Message args: ()
Returns the response vector."
   (call-next-method))

(defmeth oneway-model-proto :x ()
"Message args: ()
Returns the design matrix."
   (call-next-method))

(defmeth oneway-model-proto :intercept (&rest args)
"Message args: ()
Always returns nil. For compatibility with linear regression."
  (declare (ignore args))
  nil)

(defmeth oneway-model-proto :predictor-names () (send self :group-names))

;;;
;;; Other Methods
;;;

(defmeth oneway-model-proto :standard-deviations ()
"Message args: ()
Returns list of within group standard deviations."
  (mapcar #'standard-deviation (send self :grouped-data)))
  
(defmeth oneway-model-proto :group-df () 
"Message args: ()
Returns degrees of freedom for groups."
	(- (length (send self :grouped-data)) 1))

(defmeth oneway-model-proto :group-sum-of-squares ()
"Message args: ()
Returns sum of squares for groups."
  (sum (^ (- (send self :fit-values) (mean (send self :y))) 2)))

(defmeth oneway-model-proto :group-mean-square ()
"Message args: ()
Returns mean square for groups."
	(/ (send self :group-sum-of-squares) (send self :group-df)))
	
(defmeth oneway-model-proto :error-df ()
"Message args: ()
Returns degrees of freedom for error."
	(send self :df))
	
(defmeth oneway-model-proto :error-mean-square ()
"Message args: ()
Returna mean square for error."
	(/ (send self :sum-of-squares) (send self :df)))
	
(defmeth oneway-model-proto :boxplots ()
"Message args: ()
Produce parallel box plots of the groups."
	(boxplot (send self :grouped-data)))
	