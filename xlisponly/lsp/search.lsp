; Winston & Horn (3rd Edition) Chapter 19 


; First set up the network

(setf	(get 's 'neighbors) '(a d)
	(get 'a 'neighbors) '(s b d)
	(get 'b 'neighbors) '(a c e)
	(get 'c 'neighbors) '(b)
	(get 'd 'neighbors) '(s a e)
	(get 'e 'neighbors) '(b d f)
	(get 'f 'neighbors) '(e))

(setf	(get 's 'coordinates) '(0 3)
	(get 'a 'coordinates) '(4 6)
	(get 'b 'coordinates) '(7 6)
	(get 'c 'coordinates) '(11 6)
	(get 'd 'coordinates) '(3 0)
	(get 'e 'coordinates) '(6 0)
	(get 'f 'coordinates) '(11 3))


; The extend function is used everywhere to provide a new queue

(defun extend (path)
	(print (reverse path))		; for observing what is happening
	(mapcar #'(lambda (new-node) (cons new-node path))
	        (remove-if #'(lambda (neighbor) (member neighbor path))
			   (get (first path) 'neighbors))))

; depth first search

(defun depth-first (start finish &optional (queue (list (list start))))
	(cond	((endp queue) nil)		; Queue empty?
		((eq finish (first (first queue))) ; finish found?
		 (reverse (first queue)))
		(t (depth-first
		    start
		    finish
		    (append (extend (first queue))
		            (rest queue))))))

; breadth first search

(defun breadth-first (start finish &optional (queue (list (list start))))
	(cond	((endp queue) nil)		; Queue empty?
		((eq finish (first (first queue))) ; finish found?
		 (reverse (first queue)))
		(t (breadth-first
		    start
		    finish
		    (append (rest queue)
		            (extend (first queue)))))))

; best first search

(defun best-first (start finish &optional (queue (list (list start))))
	(cond	((endp queue) nil)		; Queue empty?
		((eq finish (first (first queue))) ; finish found?
		 (reverse (first queue)))
		(t (best-first
		    start
		    finish
		    (sort (append (extend (first queue))
		    	          (rest queue))
			  #'(lambda (p1 p2) (closerp p1 p2 finish)))))))



(defun square (x) (* x x))

(defun straight-line-distance (node-1 node-2)
	(let ((coord-1 (get node-1 'coordinates))
	      (coord-2 (get node-2 'coordinates)))
	     (sqrt (float (+ (square (- (first coord-1) (first coord-2)))
	     		     (square (- (second coord-1) (second coord-2))))))))

(defun closerp (path-1 path-2 target-node)
	(< (straight-line-distance (first path-1) target-node)
	   (straight-line-distance (first path-2) target-node)))



; hill climb search

(defun hill-climb (start finish &optional (queue (list (list start))))
	(cond	((endp queue) nil)		; Queue empty?
		((eq finish (first (first queue))) ; finish found?
		 (reverse (first queue)))
		(t (hill-climb 
		    start
		    finish
		    (append (sort (extend (first queue))
  				  #'(lambda (p1 p2) 
				           (closerp p1 p2 finish)))
			    (rest queue))))))



; branch and bound search (shortest length guarenteed)

(defun branch-and-bound (start finish &optional (queue (list (list start))))
	(cond	((endp queue) nil)		; Queue empty?
		((eq finish (first (first queue))) ; finish found?
		 (reverse (first queue)))
		(t (branch-and-bound
		    start
		    finish
		    (sort (append (extend (first queue))
		    	          (rest queue))
			  #'shorterp)))))

(defun shorterp (path-1 path-2)
	(< (path-length path-1) (path-length path-2)))

(defun path-length (path)
	(if	(endp (rest path))
		0
		(+ (straight-line-distance (first path) (second path))
		   (path-length (rest path)))))



; pert chart searching (problem 19-7)

(setf	(get 's 'successors) '(a d)
	(get 'a 'successors) '(b d)
	(get 'b 'successors) '(c e)
	(get 'c 'successors) '()
	(get 'd 'successors) '(e)
	(get 'e 'successors) '(f)
	(get 'f 'successors) '())

(setf	(get 's 'time-consumed) 3
	(get 'a 'time-consumed) 2
	(get 'b 'time-consumed) 4
	(get 'c 'time-consumed) 3
	(get 'd 'time-consumed) 3
	(get 'e 'time-consumed) 2
	(get 'f 'time-consumed) 1)

(defun pextend (path)
	(mapcar #'(lambda (new-node) (cons new-node path))
	        (remove-if #'(lambda (successor) (member successor path))
			   (get (first path) 'successors))))

(defun all-paths (start &optional (queue (list (list start))))
	(let ((extended (pextend (first queue))))
	     (cond ((endp extended)
	            (mapcar #'reverse queue))
		   (t (all-paths
		    	start
			(sort (append extended (rest queue))
			  #'first-path-incomplete-p))))))

(defun first-path-incomplete-p (p1 p2)
	(not (endp (pextend p1))))


; Pert chart searching (problem 19-8)

(defun time-consumed (path)
	(if (endp path)
	    0
	    (+ (get (first path) 'time-consumed)
	       (time-consumed (rest path)))))

(defun longerp (p1 p2) (> (time-consumed p1) (time-consumed p2)))

(defun critical-path (start &optional (queue (list (list start))))
	(let ((extended (pextend (first queue))))
	     (cond ((endp extended)
	            (reverse (first (sort queue #'longerp))))
		   (t (critical-path
		    	start
			(sort (append extended (rest queue))
			  #'first-path-incomplete-p))))))
