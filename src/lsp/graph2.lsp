(provide "graph2")
(require "graphics")

;;;;
;;;;
;;;; Scatmat Object Prototype
;;;;
;;;;

(send scatmat-proto :title "Scatterplot Matrix")
(send scatmat-proto :menu-title "Scatmat")
(send scatmat-proto :fixed-aspect t)
(send scatmat-proto :menu-template (send graph-proto :menu-template))

(defmeth scatmat-proto :redraw-background () (send self :erase-window))


;;;;
;;;;
;;;; Spinner Object Prototype
;;;;
;;;;

(send spin-proto :title "Spinning Plot")
(send spin-proto :menu-title "Spinner")
(send spin-proto :fixed-aspect t)
(send spin-proto :menu-template (append (send graph-proto :menu-template)
                                        '(dash faster slower cuing axes)))
(send spin-proto :variable-labels '("X" "Y" "Z"))
(send spin-proto :black-on-white nil)
(send spin-proto :depth-cuing t)
(send spin-proto :showing-axes t)
(send spin-proto :scale-type 'variable :draw nil)
                                        
(defmeth spin-proto :isnew (&rest args)
  (apply #'call-next-method args)
  (send self :add-overlay (send spin-control-overlay-proto :new)))

(defmeth spin-proto :adjust-to-data (&key (draw t))
  (call-next-method :draw nil)
  (when (null (send self :scale-type))
        (let* ((vars (send self :num-variables))
               (ranges (send self :range (iseq 0 (- vars 1))))
               (radius (* (sqrt vars)
                          (max (- (min ranges)) (max ranges)))))
          (send self :center (iseq vars) 0 :draw nil)
          (send self :range (iseq vars) (- radius) radius :draw nil)))
  (when draw
        (send self :resize)
        (send self :redraw)))

(defmeth spin-proto :rotation-type (&optional (new nil set))
  (if set (setf (slot-value 'rotation-type) new))
  (slot-value 'rotation-type))

(defmeth spin-proto :make-menu-item (item)
  (if (symbolp item)
      (case item
        (faster (send spin-speed-item-proto :new self 1.5))
        (slower (send spin-speed-item-proto :new self (/ 2 3)))
        (cuing  (send graph-item-proto :new "Depth Cuing" self
                      :depth-cuing :depth-cuing :toggle t :redraw t))
        (axes   (send graph-item-proto :new "Show Axes" self
                      :showing-axes :showing-axes :toggle t :redraw t))
        (t (call-next-method item)))
      item))
      
(defmeth spin-proto :add-surface (x y z &key (draw t) (type 'solid) (spline 3)
				    color)
"Args: (x y z &key (draw t) (type 'solid) (spline 3) color)
Adds a grid surface using sequences X, Y with values in the matrix Z.
Z should be (length X) by (length Y)."
  (let ((z (row-list z)))
    (mapcar #'(lambda (u z) 
               (let* ((yz (if spline
                              (spline y z :xvals (* spline (length y)))
                              (list y z)))
                      (y (first yz))
                      (z (second yz)))
                 (send self
                       :add-lines 
                       (list (repeat u (length y)) y z) 
                       :draw nil :type type :color color)))
            x z))
  (let ((z (column-list z)))
    (mapcar #'(lambda (u z)
                (let* ((xz (if spline 
                               (spline x z :xvals (* spline (length x)))
                               (list x z)))
                       (x (first xz))
                       (z (second xz)))
                  (send self
                        :add-lines 
                        (list x (repeat u (length x)) z)
                        :draw nil :type type :color color)))
            y z))
  (if draw (send self :redraw))
  nil)
  
(defmeth spin-proto :add-function (f xmin xmax ymin ymax &rest args &key (num-points 6))
"Args: (f xmin xmax ymin ymax &rest args &key (num-points 6))
Adds surface of function F over a NUM-POINTS by NUM-POINTS grid on the
rectangle [xmin, xmax] x [ymin, ymax]. Passes other keywords to
:add-surface method."
  (let* ((x (rseq xmin xmax num-points))
         (y (rseq ymin ymax num-points))
         (z (outer-product x y f)))
    (apply #'send self :add-surface x y z args)))
  
(defmeth spin-proto :abcplane (a b c &rest args)
"Message args: (a b c)
Adds the graph of the plane A + B x + Cy to the plot."
  (let ((xlimits (send self :range 0))
        (ylimits (send self :range 1)))
    (apply #'send self :add-function #'(lambda (x y) (+ a (* b x) (* c y)))
	   (- (mean xlimits) (/ (abs (apply #'- xlimits)) (* 2 (sqrt 3))))
	   (+ (mean xlimits) (/ (abs (apply #'- xlimits)) (* 2 (sqrt 3))))
	   (- (mean ylimits) (/ (abs (apply #'- ylimits)) (* 2 (sqrt 3))))
	   (+ (mean ylimits) (/ (abs (apply #'- ylimits)) (* 2 (sqrt 3))))
	   :spline nil
	   args)))

;;
;; Spinner control overlay
;;

(defproto spin-control-overlay-proto 
          '(top lefts gap side ascent box-top text-base)
          ()
          graph-overlay-proto)

(defmeth spin-control-overlay-proto :isnew ()
  (setf (slot-value 'gap) 5)
  (setf (slot-value 'side) 10)
  (setf (slot-value 'ascent) (send graph-proto :text-ascent))
  (let ((w1 (send graph-proto :text-width "Pitch"))
        (w2 (send graph-proto :text-width "Roll"))
        (w3 (send graph-proto :text-width "Yaw"))
        (gap (slot-value 'gap))
        (side (slot-value 'side)))
    (setf (slot-value 'lefts)
          (list (* 2 gap)
                (+ (* 3 gap) side)
                (+ (* 6 gap) (* 2 side) w1)
                (+ (* 7 gap) (* 3 side) w1)
                (+ (* 11 gap) (* 4 side) w1 w2)
                (+ (* 12 gap) (* 5 side) w1 w2)))))

(defmeth spin-control-overlay-proto :resize ()
  (let* ((graph (send self :graph))
         (height (send graph :canvas-height))
         (bottom-margin (fourth (send graph :margin)))
         (top (+ (- height bottom-margin) 1))
         (gap (slot-value 'gap))
         (side (slot-value 'side))
         (ascent (send graph :text-ascent))
         (text-base (+ top gap (max side ascent)))
         (box-top (- text-base side)))
    (setf (slot-value 'top) top)
    (setf (slot-value 'text-base) text-base)
    (setf (slot-value 'box-top) box-top)))

(defmeth spin-control-overlay-proto :redraw ()
  (let ((graph (slot-value 'graph))
        (top (slot-value 'top))
        (lefts (slot-value 'lefts))
        (gap (slot-value 'gap))
        (side (slot-value 'side))
        (text-base (slot-value 'text-base))
        (box-top (slot-value 'box-top)))
    (send graph :draw-line 0 top (send graph :canvas-width) top)
    (mapcar #'(lambda (x) (send graph :frame-rect x box-top side side))
            lefts)
    (mapcar #'(lambda (s x y) (send graph :draw-string s x y))
            '("Pitch" "Roll" "Yaw")
            (+ (select lefts '(1 3 5)) gap side) 
            (repeat text-base 3))))

(defmeth spin-control-overlay-proto :do-click (x y m1 m2)
  (declare (ignore m2))
  (let ((graph (slot-value 'graph))
        (top (slot-value 'top))
        (lefts (slot-value 'lefts))
        (gap (slot-value 'gap))
        (side (slot-value 'side))
        (text-base (slot-value 'text-base))
        (box-top (slot-value 'box-top)))
    (when (< top y)
          (send graph :idle-on nil)
          (if (< box-top y text-base)
              (let ((i (car (which (< lefts x (+ lefts side)))))
                    (angle (abs (send graph :angle))))
                (when i
                      (send graph :rotation-type 
                            (select '(pitching rolling yawing)
                                    (floor (/ i 2))))
                      (send graph :angle (if (oddp i) angle (- angle)))
                      (send graph :while-button-down
                            #'(lambda (x y) (send graph :rotate) (pause 2))
                            nil)
                      (send graph :idle-on m1))))
          t)))


;;
;; Spinner Menu Items
;;

;; SPIN-SPEED-ITEM-PROTO. multiply speed by fixed number to speed up or slow down. 
(defproto spin-speed-item-proto '(graph mult) () menu-item-proto)

(defmeth spin-speed-item-proto :isnew (v m)
  (setf (slot-value 'graph) v)
  (setf (slot-value 'mult) m)
  (call-next-method (if (> 1 m) "Slower" "Faster"))
  (send self :key (if (> 1 m) #\S #\F)))
  
(defmeth spin-speed-item-proto :do-action ()
  (send (slot-value 'graph) :angle (* (slot-value 'mult) (send (slot-value 'graph) :angle))))

;;
;; Patch to pause between rotation frames in idle action
;;

;; **** better approach would be to use a timer so multiple spin plots
;; **** don't slow each other down.

(when (eq (type-of (send spin-proto :get-method :do-idle)) 'SUBR)
  (setf xlisp::old-spin-idle (send spin-proto :get-method :do-idle))
  (defmeth spin-proto :do-idle ()
    (funcall xlisp::old-spin-idle self)
    (pause 2)))


;;;;
;;;;
;;;; Spinner Functions
;;;;
;;;;

(defun spin-function (f xmin xmax ymin ymax &rest args)
"Args: (f xmin xmax ymin ymax &key (num-points 6) (spline 3))
Rotatable plot of function F of two real variables over the range
between [xmin, xmax] x [ymin, ymax]. The function is evaluated at
NUM-POINTS points. If SPLINE is not NIL a spline is fit at 
(* SPLINE NUMPOINTS) points."
  (let ((plot (apply #'send spin-proto :new 3 :show nil args)))
    (apply #'send plot :add-function f xmin xmax ymin ymax :draw nil args)
    (send plot :adjust-to-data :draw nil)
    (send plot :new-menu)
    (send plot :showing-axes nil)
    (send plot :rotate-2 0 1 (/ pi 3) :draw nil)
    (send plot :rotate-2 1 2 (- (/ pi 3)) :draw nil)
    (send plot :show-window)
    plot))

;;;;
;;;;
;;;; Name List Object Prototype
;;;;
;;;;

(send name-list-proto :title "Name List")
(send name-list-proto :menu-title "List")
(send name-list-proto :menu-template '(link mouse dash erase-selection 
                                            focus-on-selection show-all
					    color
                                            selection dash options
#+unix                                      save-image))

(defmeth name-list-proto :clear-content ()
    (apply #'send self :erase-rect (send self :view-rect)))


;;;;
;;;;
;;;; Histogram Object Prototype
;;;;
;;;;

(send histogram-proto :title "Histogram")
(send histogram-proto :menu-title "Histogram")
(send histogram-proto :fixed-aspect nil)
(send histogram-proto :size 250 125)
(send histogram-proto :menu-template '(link mouse resize-brush dash 
                                            erase-selection
                                            focus-on-selection show-all
					    color
                                            selection dash 
					    slicer 
                                            rescale 
					    options 
#+unix                                      save-image
                                            dash change-bins))

(defmeth histogram-proto :make-menu-item (item)
  (if (symbolp item)
      (case item
        (change-bins (send change-hist-bins-item-proto :new self))
        (t (call-next-method item)))
      item))
      
(defmeth histogram-proto :drag-point (x y &key (draw t))
  (let ((p (call-next-method x y :draw nil)))
    (if p (send self :resize))
    (if (and p draw) (send self :redraw))
    p))

;;
;; Histogram Menu Items
;;

;; CHANGE-HIST-BINS-ITEM-PROTO. Opens new integer dialog.
(defproto change-hist-bins-item-proto '(graph) () menu-item-proto)

(defmeth change-hist-bins-item-proto :isnew (h)
  (setf (slot-value 'graph) h)
  (call-next-method "Change Bins"))

(defmeth change-hist-bins-item-proto :do-action ()
  (let ((bins (get-new-integer "Number of bins"
                               2
                               30
                               (send (slot-value 'graph) :num-bins))))
    (when bins 
          (send (slot-value 'graph) :num-bins bins)
          (send (slot-value 'graph) :redraw))))

;;;;
;;;;
;;;; Scatterplot Object Prototype
;;;;
;;;;

(send scatterplot-proto :title "Plot")
(send scatterplot-proto :menu-title "Plot")
(send scatterplot-proto :fixed-aspect nil)
(send scatterplot-proto :menu-template (send graph-proto :menu-template))

(defmeth graph-proto :add-function (f xmin xmax &rest args &key (num-points 50))
"Message args: (f xmin xmax &key (num-points 50)
Adds plot of function F of one real variable over the range between xmin
and xmax to the plot. The function is evaluated at NUM-POINTS points."
  (unless (= 2 (send self :num-variables)) (error "only works for 2D plots"))
  (let* ((x (rseq xmin xmax num-points))
         (y (mapcar f x)))
    (apply #'send self :add-lines (list x y) args)))

(defmeth graph-proto :abline (a b)
"Message args: (a b)
Adds the graph of the line A + B x to the plot."
  (let ((limits (send self :range 0)))
    (send self :add-function #'(lambda (x) (+ a (* b x)))
          (car limits)
          (cadr limits))))

(defmeth graph-proto :plotline (a b c d draw)
  (send self :add-lines (list a c) (list b d) :draw draw))

;;;;
;;;;
;;;; Basic 2D Plotting Functions
;;;;
;;;;

(defun plot-function (f xmin xmax &key (num-points 50) (type 'solid) labels)
"Args: (f xmin xmax &optional (num-points 50) labels)
Plots function F of one real variable over the range between xmin and xmax.
The function is evaluated at NUM-POINTS points. LABELS is a list of axis
labels."
  (let* ((x (rseq xmin xmax num-points))
         (y (mapcar f x)))
    (plot-lines x y :type type :variable-labels labels)))

;;;;
;;;;
;;;; Boxplot  Functions
;;;;
;;;;

(defmeth scatterplot-proto :add-boxplot (y &key (x 1.0) (width 1.0) (draw t))
  (unless (= 2 (send self :num-variables)) (error "only works for 2D plots"))
  (let* ((half-box (* 0.4 width))
         (half-foot (* 0.1 width))
         (fiv (fivnum y))
         (low (select fiv 0))
         (q1 (select fiv 1))
         (med (select fiv 2))
         (q3 (select fiv 3))
         (high (select fiv 4)))
    (send self :plotline (- x half-foot) low  (+ x half-foot) low  nil)
    (send self :plotline (- x half-foot) high (+ x half-foot) high nil)
    (send self :plotline x low x q1   nil)
    (send self :plotline x q3  x high nil)
    (send self :plotline (- x half-box) q1  (+ x half-box) q1  nil)
    (send self :plotline (- x half-box) med (+ x half-box) med nil)
    (send self :plotline (- x half-box) q3  (+ x half-box) q3  nil)
    (send self :plotline (- x half-box) q1  (- x half-box) q3  nil)
    (send self :plotline (+ x half-box) q1  (+ x half-box) q3  nil)))

(defun boxplot (data &key (title "Box Plot"))
"Args: (data &key (title \"Box Plot\"))
DATA is a sequence, a list of sequences or a matrix. Makes a boxplot of the
sequence or a parallel box plot of the sequences in the list or the columns
of the matrix." 
  (let ((p (send scatterplot-proto :new 2 :title title :show nil)))
    (setq data 
          (cond ((matrixp data) (column-list data))
                ((or (not (listp data)) (numberp (car data))) (list data))
                (t data)))
        (let ((range (get-nice-range (min data) (max data) 4)))
          (send p :range 1 (nth 0 range) (nth 1 range))
          (send p :y-axis t nil (nth 2 range)))
    (send p :range 0 0 (1+ (length data)))
    (dotimes (i (length data))
          (send p :add-boxplot (nth i data) :x (1+ i)))
    (send p :show-window)
    p))

(defun boxplot-x (x data &key (title "Box Plot"))
"Args: (x data &key (title \"Box Plot\"))
DATA is a list of sequences or a matrix. X is a sequence with as many
elements as DATA has elements or columns. Makes a parallel box plot
of the sequences in the list or the columns of the matrix vs X." 
  (let ((p (send scatterplot-proto :new 2 :title title :show nil)))
    (setq data 
          (cond ((matrixp data) (column-list data))
                ((or (not (listp data)) (numberp (car data))) (list data))
                (t data)))
        (let ((range (get-nice-range (min data) (max data) 4)))
          (send p :range 1 (nth 0 range) (nth 1 range))
          (send p :y-axis t nil (nth 2 range)))
    (setq x (coerce x 'list))
    (if (/= (length x) (length data)) (error "argument lengths do not match"))
    (let* ((width (min (difference x)))
           (range (get-nice-range (- (min x) width) (+ (max x) width) 4)))
      (send p :range 0 (nth 0 range) (nth 1 range))
      (send p :x-axis t nil (nth 2 range))
      (dotimes (i (length data))
               (send p :add-boxplot (nth i data) :width width :x (nth i x))))
    (send p :show-window)
    p))

;;;;
;;;;
;;;; Quantile and Probability Plot Functions
;;;;
;;;;

(defun quantile-plot (x &key (quantile-function #'normal-quant) 
                        (title "Quantile Plot")
			point-labels
			(variable-labels
			 '("Theoretical Quantiles" "Observed Quantiles")))
"Args: (data &key (quantile-function #'normal-quant) (title \"Quantile Plot\") point-labels)"
  (plot-points (funcall quantile-function
                        (/ (1+ (rank x)) (1+ (length x))))
               x
               :title title 
               :variable-labels variable-labels
               :point-labels point-labels))

(defun probability-plot (x &key (distribution-function #'normal-cdf)
                           (title "Probability Plot") point-labels)
"Args: (data &key (distribution-function #'normal-cdf) (title \"Probability Plot\") point-labels)"
  (let ((p (plot-points (/ (1+ (rank x)) (1+ (length x)))
                                (funcall distribution-function x)
                                :title title
                                :variable-labels '("Theoretical CDF" "Observed CDF")
                                :point-labels point-labels)))
    (send p :x-axis t t 5)
    (send p :y-axis t t 5)
    p))

;;;;
;;;;
;;;; Contour Plotting Methods and Functions
;;;;
;;;;

(defmeth scatterplot-proto :add-surface-contour (x y z v &key (draw t))
  (let ((c (surface-contour x y z v)))
    (dolist (x (split-list c 2))
            (send self :add-lines (transpose x) :draw nil)))
    (if draw (send self :redraw-content)))
    
(defmeth scatterplot-proto :add-surface-contours 
         (x y z &optional v &key (draw t))
  (let ((v (if v 
               (if (numberp v) (list v v) (coerce v 'list))
               (let ((min (min z))
                     (max (max z)))
                 (+ min (* (- max min) '(.2 .4 .6 .8)))))))
    (dolist (v v) (send self :add-surface-contour x y z v :draw nil))
    (if draw (send self :redraw-content))))

(defmeth scatterplot-proto :add-function-contours 
         (f xmin xmax ymin ymax &optional v &key (num-points 6) (draw t))
  (let* ((x (coerce (rseq xmin xmax num-points) 'vector))
         (y (coerce (rseq ymin ymax num-points) 'vector))
         (z (outer-product x y f)))
    (send self :add-surface-contours x y z v :draw draw)))

(defun contour-function (f xmin xmax ymin ymax &rest args
                           &key levels (num-points 6))
"Args: (f xmin xmax ymin ymax &key levels (num-points 6))
Contour plot of function F of two real variables over the range
between [xmin, xmax] x [ymin, ymax]. The function is evaluated at
NUM-POINTS points."
  (let ((plot (apply #'send scatterplot-proto :new 2 :show nil args)))
    (send plot :add-function-contours f xmin xmax ymin ymax 
          levels :num-points num-points :draw nil)
    (send plot :adjust-to-data :draw nil)
    (send plot :new-menu)
    (send plot :show-window)
    plot))

(require "graph3")
