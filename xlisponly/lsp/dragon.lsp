;; DRAGON.L FOR PC-LISP V2.10
;; Modified for xlisp 2.1d (w. graphics extensions) by Tom Almy
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~ 
;;     Draw an Nth order Dragon Curve requires Turtle.l routines to run.
;; Taken From Byte April 1986. Try (DragonCurve 16) then put on supper,
;; watch the news and come back in an hour and see the results. It takes 
;; about 1/2 hour on my machine so on a normal IBM-PC it should take about
;; an 1.5 hours.
;;
;;              Peter Ashwood-Smith.
;;              April 1986
;;
;;              P.S - This dragon is nicknamed "spot"

#-:turtle (load "turtle")

(defvar *StepSize* 1)

(defun Dragon(sign level)
       (if    (zerop level) 
       	      (TurtleForward *StepSize*)
              (progn
		(setq level (1- level))
		(TurtleRight (* 45 sign))
		(Dragon -1 level)
		(TurtleLeft (* 90 sign))
		(Dragon 1 level)
		(TurtleRight (* 45 sign))
              )
       )
)

(defun DragonCurve (n m)
       (setq *StepSize* m)                   ; *StepSize* is global variable
       (TurtleGraphicsUp)
       (TurtleCenter)
       (TurtleGoto 50 50)
       (TurtleRight 30)                          ; angle the serpent a bit
       (Dragon 1 n)
       (gc)
)

(print "Try (DragonCurve 14 1)")
