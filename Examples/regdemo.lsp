
;; set up some simulated data
(def x (append (iseq 1 18) (list 30 40)))
(def y (+ x (* 2 (normal-rand 20))))

;; construct the plot
(def myplot (plot-points x y))

;; add a new "mouse mode", with menu title, cusror and mouse method name
(send myplot :add-mouse-mode 'point-moving
      :title "Point Moving"
      :cursor 'finger
      :click :do-point-moving)

;; add the new mouse method
(defmeth myplot :do-point-moving (x y a b)
  (let ((p (send self :drag-point x y :draw nil)))
    (if p (send self :set-regression-line))))

;; add method for drawing the regression line for the current data
(defmeth myplot :set-regression-line ()
  (let ((coefs (send self :calculate-coefficients)))
    (send self :clear-lines :draw nil)
    (send self :abline (select coefs 0) (select coefs 1))))

;; add method for calculating regression coefficients for current data
(defmeth myplot :calculate-coefficients ()
  (let* ((i (iseq 0 (- (send self :num-points) 1)))
         (x (send self :point-coordinate 0 i))
         (y (send self :point-coordinate 1 i))
         (m (regression-model x y :print nil)))
    (send m :coef-estimates)))

;; add the regression line
(send myplot :set-regression-line)

;; put the plot in "point moving" mode
(send myplot :mouse-mode 'point-moving)
