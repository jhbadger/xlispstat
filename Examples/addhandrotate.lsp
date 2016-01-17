;; add a new "mouse mode", with menu title, cusror and mouse method name
(send spin-proto :add-mouse-mode 'hand-rotate
      :title "Hand Rotate" :cursor 'hand :click :do-hand-rotate)

;; set up local environment with function to project (x, y) point onto
;; "globe" overthe plot
(flet ((calcsphere (x y)
        (let* ((norm-2 (+ (* x x) (* y y)))
               (rad-2 (^ 1.7 2))
               (z (if (< norm-2 rad-2) (sqrt (- rad-2 norm-2)) 0)))
          (if (< norm-2 rad-2) 
              (list x y z)
              (let ((r (sqrt (max norm-2 rad-2))))
                (list (/ x r) (/ y r) (/ z r)))))))
                
  ;; define the :DO-HAND-ROTATE method in the local environment
  (defmeth spin-proto :do-hand-rotate (x y m1 m2)
    (let* ((oldp (apply #'calcsphere 
			(send self :canvas-to-scaled x y)))
	   (p oldp)
	   (vars (send self :content-variables))
	   (trans (identity-matrix (send self :num-variables))))
      (send self :idle-on nil)
      (send self :while-button-down 
	    #'(lambda (x y) 
		(setf oldp p)
		(setf p (apply #'calcsphere 
			       (send self :canvas-to-scaled x y)))
		(setf (select trans vars vars) (make-rotation oldp p))
		(when m1 
		      (send self :slot-value 'rotation-type trans)
		      (send self :idle-on t))
		(send self
		      :apply-transformation
		      trans))))))

                        


