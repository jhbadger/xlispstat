;; Sort routines.  
;; by Tom Almy


;; The built in sort does a quick sort which does a bad job if the list is
;; already sorted.  INSERT is a destructive insertion into a sorted list.
;; Also, these are iterative and will handle lists of any size.  SORT can
;; cause eval stack overflows on big lists.
;; In these functions, "function" is a predicate that orders the list
;; (For numbers, typically #'< ).
(defun insert (element list function)
	(cond ((null list) (list element))
	      ((funcall function element (first list))
	       (cons element list))
	      (t (do ((prev list (rest prev)))
	      	     ((or (endp (rest prev)) 
		     	  (funcall function element (second prev)))
		      (rplacd prev (cons element (rest prev)))
		      list)))))

;; And this inserts a list of items into an existing list (which can be nil)

(defun insertall (elements list function)
	(dolist (element elements list)
		(setq list (insert element list function))))


;; Once the list has been sorted, accessing is faster if the list is
;; placed in an array, and a binary search is performed.
;; The advantage starts at about 250 elements

(defun memarray (element array &key (test #'eql) (function #'<))
	(let* ((max (1- (length array)))
	      (min 0)
	      (index (/ (+ max min) 2)))
	     (loop (when (funcall test element (aref array index))
	     		 (return index))
		   (if (funcall function element (aref array index))
		       (setq max (1- index))
		       (setq min (1+ index)))
		   (when (> min max) (return nil))
		   (setq index (/ (+ max min) 2)))))


