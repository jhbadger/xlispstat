;; TURTLE.L for PC-LISP.EXE V2.10
;; Modified for XLISP-PLUS 2.1d by Tom Almy
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;      A set of turtle graphics primitives to demonstrate PC-LISP's BIOS 
;; graphics routines. These routines are pretty self explanitory. The first
;; 5 defun's define the primitives, next are a set of routines to draw things
;; like squares, triangles etc. Try the function (GraphicsDemo). It will
;; draw Squirals, Trianglerals, etc. Note that the BIOS line drawing is really
;; slow. This is because the BIOS 'set dot/pixel' routine is used for every
;; point in a line. Using the BIOS has the advantage however of portability,
;; these routines work on virtually every MS-DOS machine. The global variable
;; *GMODE* controls the graphics resolution that will be used. It is set by 
;; default to 6 I set it to 8 or 9 for my 2000 but these routines will not
;; support the lower resolution modes. 
;;
;;                      Peter Ashwood-Smith
;;                      April 2nd, 1986 
;;


;; Several bugs  fixed by Tom Almy
;; The playing field is 200x200, after scaling.
;; Lfactor = ypixels/200
;; Scale = xpixels/ypixels
;; CenterX=CenterY= ypixels/2



(defvar *GMODE* 18)                                     ; default setting


#+:times (defun pause (time) 
	   (let ((fintime (+ (* time internal-time-units-per-second)
			     (get-internal-run-time))))
		(loop (when (> (get-internal-run-time) fintime)
			    (return-from pause)))))
#-:times (defun pause () (dotimes (x (* time 1000))))


(defun TurtleGraphicsUp (&aux dims)           
       (setq
	dims
	(case *GMODE*
	      ((6 16 18)			; 640x200 B&W mode
						; 640x350 Graphics
					        ; 640x480 VGA Graphics
	       (mode *GMODE*))
	      (t (error "unsupported *GMODE* - ~s" *GMODE*))))
       (setq  Lfactor (/ (1+ (fourth dims)) 200)
	      Scale   (/ (1+ (third dims)) (1+ (fourth dims)))
	      CenterX (/ (1+ (fourth dims)) 2)
	      CenterY CenterX
	      Lastx CenterX
	      Lasty CenterY
	      Heading 0)
       (cls)
       (color 15)
)   

(defun TurtleGraphicsDown() 
	(mode 3) (cls))
(defun TurtleCenter()       
	(setq Lastx CenterX Lasty CenterY Heading 1.570796372))
(defun TurtleRight(n)       (setq Heading (- Heading (* n 0.01745329))))
(defun TurtleLeft(n)        (setq Heading (+ Heading (* n 0.01745329))))
(defun TurtleGoto(x y)      (setq Lastx (* x Lfactor) Lasty (* y Lfactor) )) 

(defun TurtleForward(n) 
      (setq n (* n Lfactor) 
      	    Newx (+ Lastx (* (cos Heading) n))
	    Newy (+ Lasty (* (sin Heading) n)))
      (move (truncate (* Lastx Scale))
            (truncate Lasty)
	    (truncate (* Newx Scale))
	    (truncate Newy))
      (setq Lastx Newx Lasty Newy)
)

;
; end of Turtle Graphics primitives, start of Graphics demonstration code
; you can cut this out if you like and leave the Turtle primitives intact.
;

(defun Line_T(n)        
	(TurtleForward n) (TurtleRight 180)
	(TurtleForward (/ n 4)) 
)
	
(defun Square(n)
	(TurtleForward n)  (TurtleRight 90)     
	(TurtleForward n)  (TurtleRight 90)     
	(TurtleForward n)  (TurtleRight 90)     
	(TurtleForward n)                       
)

(defun Triangle(n)
	(TurtleForward n)  (TurtleRight 120)
	(TurtleForward n)  (TurtleRight 120)
	(TurtleForward n)
)

(defun Make(ObjectFunc Size star skew) 
      (dotimes (dummy star)
	   (apply ObjectFunc (list Size)) 
	   (TurtleRight skew)
       )
)

(defun GraphicsDemo()
	   (TurtleGraphicsUp) 
	   (Make #'Square 40 18 5) (Make #'Square 60 18 5)
	   (pause 1.0)
	   (TurtleGraphicsUp) 
	   (Make #'Triangle 40 18 5) (Make #'Triangle 60 18 5)
	   (pause 1.0)
	   (TurtleGraphicsUp) 
	   (Make #'Line_T 80 50 10)
	   (pause 1.0)
	   (TurtleGraphicsDown)
)

(print "Try (GraphicsDemo)")

(setq *features* (cons :turtle *features*))
