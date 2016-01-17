; Blocks World from Winston&Horn

#-:classes (load "classes")

; abstract classes for ball types

; basic blocks support nothing
(defclass basic-block (name width height position supported-by))

(defmethod basic-block :support-for () nil)

(defmethod basic-block :top-location  () 
	(list (+ (first position) (/ width 2))
	      (+ (second position) height)))

; movable-blocks can be moved
(defclass movable-block () () basic-block)

; load-bearing blocks can support other blocks, and can be moved
(defclass load-bearing-block (support-for) () movable-block)

; we can't have multiple inheritance, so we need a separate class for table
; table blocks can support other blocks but cannot be moved.

(defclass table-block (support-for) () basic-block)

; Specific classes for table brick wedge and ball

(defclass brick () () load-bearing-block)

(defclass wedge () () movable-block)

(defclass ball  () () movable-block)

(defclass hand  (name position grasping))


; define all the individual blocks

(setf *blocks*
      (list
        (send table-block :new :name 'table :width 20 :height 0 :position '(0 0))
	(send brick :new :name 'b1 :width 2 :height 2 :position '(0 0))
	(send brick :new :name 'b2 :width 2 :height 2 :position '(2 0))
	(send brick :new :name 'b3 :width 4 :height 4 :position '(4 0))
	(send brick :new :name 'b4 :width 2 :height 2 :position '(8 0))
	(send wedge :new :name 'w5 :width 2 :height 4 :position '(10 0))
	(send brick :new :name 'b6 :width 4 :height 2 :position '(12 0))
	(send wedge :new :name 'w7 :width 2 :height 2 :position '(16 0))
	(send ball  :new :name 'l8 :width 2 :height 2 :position '(18 0))
       ))

(dolist (l *blocks*) (set (send l :name) l))


(dolist (l (cdr *blocks*)) ; all but table block
	(setf (send table :support-for) 
	      (cons l (send table :support-for))
	      (send l :supported-by)
	      table))

(definst hand *hand* :name '*hand* :position '(0 6))

(defmethod movable-block :put-on (support)
	(if (send self :get-space support)
	    (and (send *hand* :grasp self)
	    	 (send *hand* :move  self support)
		 (send *hand* :ungrasp self))
	    (format t 
	    	    "Sorry, there is no room for ~a on ~a.~%"
		    name
		    (send support :name))))

(defmethod movable-block :get-space (support)
	(or (send self :find-space support)
	    (send self :make-space support)))

(defmethod hand :grasp (obj)
	(unless (eq grasping obj)
		(when (send obj :support-for)
		      (send obj :clear-top))
		(when grasping
		      (send grasping :rid-of))
		(setf position (send obj :top-location))
		(format t
			"Move hand to pick up ~a at location ~a.~%"
			(send obj :name)
			position)
		(format t
			"Grasp ~a.~%"
			(send obj :name))
		(setf grasping obj))
	t)

(defmethod hand :ungrasp (obj)
	(when (send obj :supported-by)
	      (format t
	      	      "Ungrasp ~a~%"
		      (send obj :name))
	      (setf grasping nil)
	      t))

(defmethod movable-block :rid-of ()
	(send self :put-on table))

(defmethod movable-block :make-space (support)
	(dolist (obstruction (send support :support-for))
		(send obstruction :rid-of)
		(let ((space (send self :find-space support)))
		     (when space (return space)))))

(defmethod  load-bearing-block :clear-top ()
	(dolist (obstacle support-for) (send obstacle :rid-of))
	t)


(defmethod hand :move (obj support)
	(send obj :remove-support)
	(let ((newplace (send obj :get-space support)))
	     (format t
	     	     "Move ~a to top of ~a at location ~a.~%"
		     (send obj :name)
		     (send support :name)
		     newplace)
	     (setf (send obj :position) newplace)
	     (setf position (send obj :top-location)))
	(send support :add-support obj)
	t)


; remove-support-for is defined twice, for each load bearing class

(defmethod load-bearing-block :remove-support-for (obj)
	(setf support-for (remove obj support-for))
	t)

(defmethod table-block :remove-support-for (obj)
	(setf support-for (remove obj support-for))
	t)

(defmethod movable-block :remove-support ()
	(when supported-by
	      (format t
		      "Removing support relations between ~a and ~a.~%"
		      (send supported-by :name)
		      name)
	      (send supported-by :remove-support-for self)
	      (setf supported-by nil))
	t)

(defmethod load-bearing-block :add-support (obj)
	(format t
		"Adding support relations between ~a and ~a.~%"
		(send obj :name)
		name)
	(setf support-for 
	      (cons obj support-for)
	      (send obj :supported-by) 
	      self)
	t)

(defmethod table-block :add-support (obj)
	(format t
		"Adding support relations between ~a and ~a.~%"
		(send obj :name)
		name)
	(setf support-for 
	      (cons obj support-for)
	      (send obj :supported-by) 
	      self)
	t)

(defmethod basic-block :add-support (obj)
	t)

(defmethod movable-block :find-space (support)
	(dotimes (offset (1+ (- (send support :width) width)))
		 (unless (intersections-p self offset
		 			  (first (send support :position))
					  (send support :support-for))
			 (return (list (+ offset (first (send support 
			 				      :position)))
				       (+ (second (send support :position))
				          (send support :height)))))))

(defun intersections-p (obj offset base obstacles)
	(dolist (obstacle obstacles)
		(let* ((ls-proposed (+ offset base))
			(rs-proposed (+ ls-proposed (send obj :width)))
			(ls-obstacle (first (send obstacle :position)))
			(rs-obstacle (+ ls-obstacle (send obstacle :width))))
		      (unless (or (>= ls-proposed rs-obstacle)
		      		  (<= rs-proposed ls-obstacle))
			      (return t)))))

