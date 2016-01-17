#-:classes (load "classes")

; Make the class ship and its instance variables be known

(defclass ship ((x 0) (y 0) (xv 0) (yv 0) (mass 0) (name 'unknown) 
	(captain 'unknown) (registry 'unknown)))

(defmethod ship :sail (time) 
	; the METHOD for sailing
	(princ (list "sailing for " time " hours\n"))
	   ; note that this form is expressed in terms of objects:  "self"
	   ; is bound to the object being talked to during the execution
	   ; of its message.  It can ask itself to do things.
	   (setf (send self :x)
	   	 (+  (send self :x) (* (send self :xv) time)))
	   ; This form performs a parallel action to the above, but more
	   ; efficiently, and in this instance, more clearly
	   (setq y (+ y (* yv time)))
	   ; Cute message for return value.  Tee Hee.
	   "Sailing, sailing, over the bountiful chow mein...")

; <a SHIP: #12345667> is not terribly instructive.  How about a more
; informative print routine?

(defmethod ship :print () (princ (list
				"SHIP NAME: " (send self :name) "\n"
				"REGISTRY: " (send self :registry) "\n"
				"CAPTAIN IS: " (send self :captain) "\n"
				"MASS IS: " (send self :mass) " TONNES\n"
				"CURRENT POSITION IS: " 
					(send self :x)	" X BY "
					(send self :y)	" Y\n"
				"SPEED IS: "
					(send self :xv)	" XV BY "
					(send self :yv)	" YV\n") ) )


; and an example object.

(definst ship Bounty :mass 50 
		     :name 'Bounty 
		     :registry 'England 
		     :captain 'Bligh)

(send Bounty :print)

(definst ship lollipop :mass (+ 10 20) :captain 'Temple :x 1000 :y 2000)

(send lollipop :print)

(definst ship hard :mass 1000 :captain 'Bozo :registry 'North-pole )

(send hard :print)
