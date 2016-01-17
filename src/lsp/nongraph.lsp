;;;;
;;;;
;;;; Replacement Plotting Functions
;;;;
;;;;

(setf (symbol-function 'plot-points) #'gnu-plot-points)
(setf (symbol-function 'plot-lines) #'gnu-plot-lines)


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
;;;; Quantile and Probability Plot Functions
;;;;
;;;;

(defun quantile-plot (x &key (quantile-function #'normal-quant) 
                        (title "Quantile Plot") point-labels)
"Args: (data &key (quantile-function #'normal-quant) (title \"Quantile Plot\") point-labels)"
  (plot-points (funcall quantile-function (/ (1+ (rank x)) (1+ (length x))))
               x))

(defun probability-plot (x &key (distribution-function #'normal-cdf)
                           (title "Probability Plot") point-labels)
"Args: (data &key (distribution-function #'normal-cdf) (title \"Probability Plot\") point-labels)"
  (plot-points (/ (1+ (rank x)) (1+ (length x)))
	       (funcall distribution-function x)))

;;;;
;;;; Disable everythinmg else
;;;;
(defmacro defnongraph (sym)
  `(defun ,sym (&rest args)
     (error "~a is not available without windows" ',sym)))

(defnongraph ok-or-cancel-dialog)
(defnongraph message-dialog)
(defnongraph get-string-dialog)
(defnongraph get-value-dialog)
(defnongraph choose-item-dialog)
(defnongraph choose-subset-dialog)
(defnongraph sequence-slider-dialog)
(defnongraph interval-slider-dialog)
(defnongraph close-all-plots)
(defnongraph get-new-integer)
(defnongraph linked-plots)
(defnongraph active-graph-windows)
(defnongraph color-symbols)
(defnongraph cursor-symbols)
(defnongraph plot-symbol-symbols)
(defnongraph pause)
(defnongraph link-views)
(defnongraph unlink-views)
(defnongraph spin-function)
(defnongraph boxplot)
(defnongraph boxplot-x)
(defnongraph contour-function)
