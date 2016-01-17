;; define a function to compute the Box-Cox transformation
(defun bc (x c p)
  (let* ((x (- x c))
         (bcx (if (< (abs p) .0001)
                  (log x)
                  (/ (^ x p) p)))
         (min (min bcx))
         (max (max bcx)))
    (/ (- bcx min) (- max min))))


;; get a sorted sample from a shi-squared distribution
(def x (sort-data (chisq-rand 30 4)))

;; compute the normal quantiles of the expected uniform order statistics
(def r (normal-quant (/ (iseq 1 30) 31)))

;; construct an initial plot without transformation
(def myplot (plot-points r (bc x 0 1)))

;;;
;;; First approach: compute as needed
;;;
;; construct a dialog for scrolling through powers and recomputing the
;; plot
#|
(interval-slider-dialog (list -1 2)
                        :points 20
                        :action #'(lambda (p)
                                   (send myplot :clear nil)
                                   (send myplot 
                                         :add-points r (bc x 0 p))))
|#
;;;
;;; Second aproach: precompute
;;;
;; construct a list of powers
(def powers (rseq -1 2 16))

;; compute transformed data for each power
(def xlist (mapcar #'(lambda (p) (bc x 0 p)) powers))

;; construct a dialog for scrolling through the list of data sets
;; and redrawing the plot
(sequence-slider-dialog xlist
                        :display powers
                        :action #'(lambda (x)
                                   (send myplot :clear nil)
                                   (send myplot :add-points r x)))

