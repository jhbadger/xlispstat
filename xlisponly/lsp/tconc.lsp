;; Not part of Common Lisp, but used in XLISP internally for string streams

;; THE CAR OF A TCONC POINTS TO THE TCONC LIST,
;; THE TAIL POINTS TO LAST ELEMENT

(defun make-tconc nil
    (cons 'nil 'nil))

(defun tconc (tc new)
    (let ((newl (cons new 'nil)))
      (if (null (cdr tc))
	  (rplaca tc newl)
	  (rplacd (cdr tc) newl))
      (rplacd tc newl)
      tc))

(defun lconc (tc list)
    (cond ((not (null list))
	   (if (null (cdr tc))
	       (rplaca tc list)
	       (rplacd (cdr tc) list))
	   (rplacd tc (last list))))
    tc)

(defun remove-head (tc)
    (cond ((null (car tc)) 'nil)
	  ((null (cdar tc))
	   (let ((element (caar tc)))
	     (rplaca tc 'nil)
	     (rplacd tc 'nil)
	     element))
	  (t (let ((element (caar tc)))
	       (rplaca tc (cdar tc))
	       element))))
