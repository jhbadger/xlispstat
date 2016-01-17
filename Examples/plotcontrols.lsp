(defmeth graph-proto :add-control (c) (send self :add-overlay c))
(defmeth graph-proto :delete-control (c) (send self :delete-overlay c))

(defproto graph-control-proto 
  '(action location title) nil graph-overlay-proto)

(defmeth graph-control-proto :location (&optional (new nil set))
  (when set
        (send self :erase)
        (setf (slot-value 'location) new)
        (send self :redraw))
  (slot-value 'location))

(defmeth graph-control-proto :title (&optional (new nil set))
  (when set
        (send self :erase)
        (setf (slot-value 'title) new)
        (send self :redraw))
  (slot-value 'title))

(defmeth graph-control-proto :erase ()
  (let ((graph (send self :graph))
        (loc (send self :location))
        (sz (send self :size)))
    (if graph (apply #'send graph :erase-rect (append loc sz)))))

(defmeth graph-control-proto :size () 
  (let ((graph (send self :graph))
        (title (send self :title)))
    (if graph
        (list (+ 10 5 (send graph :text-width title)) 20)
        (list 10 10))))

(defmeth graph-control-proto :redraw ()
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x (first loc))
         (loc-y (second loc))
         (title (send self :title)))
    (send self :erase)
    (send graph :frame-rect loc-x (+ 5 loc-y) 10 10)
    (send graph :draw-text title (+ 15 loc-x) (+ 15 loc-y) 0 0)))

(defmeth graph-control-proto :do-click (x y a b)
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x (first loc))
         (loc-y (+ 5 (second loc))))
    (when (and (< loc-x x (+ loc-x 10)) (< loc-y y (+ loc-y 10)))
          (send graph :paint-rect (+ 1 loc-x) (+ 1 loc-y) 8 8)
          (send self :do-action (list a b))
          (send graph :while-button-down
                #'(lambda (x y) (send self :do-action nil)) nil)
          (send graph :erase-rect (+ 1 loc-x) (+ 1 loc-y) 8 8)
          t)))

(defmeth graph-control-proto :do-action (x) (sysbeep))

;;; Rockers

(defproto rocker-control-proto () () graph-control-proto)

(defmeth rocker-control-proto :size () 
  (let ((graph (send self :graph))
        (title (send self :title)))
    (if graph
        (list (+ 10 5 10 5 (send graph :text-width title)) 20)
        (list 10 10))))

(defmeth rocker-control-proto :redraw ()
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x (first loc))
         (loc-y (second loc))
         (title (send self :title)))
    (send self :erase)
    (send graph :frame-rect loc-x (+ 5 loc-y) 10 10)
    (send graph :frame-rect (+ 15 loc-x) (+ 5 loc-y) 10 10)
    (send graph :draw-text title (+ 30 loc-x) (+ 15 loc-y) 0 0)))

(defmeth rocker-control-proto :do-click (x y a b)
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x1 (first loc))
         (loc-x2 (+ 15 loc-x1))
         (loc-y (+ 5 (second loc))))
    (if (< loc-y y (+ loc-y 10))
        (let* ((arg (cond 
                     ((< loc-x1 x (+ loc-x1 10)) '-)
                     ((< loc-x2 x (+ loc-x2 10)) '+)))
               (loc-x (case arg (- loc-x1) (+ loc-x2))))
          (when arg
                (send graph :paint-rect (+ 1 loc-x) (+ 1 loc-y) 8 8)
                (send self :do-action (list a b) arg)
                (send graph :while-button-down
                      #'(lambda (x y) (send self :do-action nil arg)) nil)
                (send graph :erase-rect (+ 1 loc-x) (+ 1 loc-y) 8 8)
                t)))))

(defmeth rocker-control-proto :do-action (x arg) (sysbeep))

;;; Slider

(defproto slider-control-proto 
  '(index sequence display) () graph-control-proto)

(defmeth slider-control-proto :isnew (sequence &key 
                                               (title "Value")
                                               (display sequence)
                                               (location '(10 20))
                                               (index 0)
                                               graph)
  (call-next-method :title title :location location)
  (send self :sequence sequence :display display)
  (send self :index index)
  (if graph (send graph :add-control self)))

(defmeth slider-control-proto :size () 
  (let ((graph (send self :graph))
        (title (send self :title)))
    (list 100 30)))

(defmeth slider-control-proto :redraw ()
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x (first loc))
         (loc-y (second loc))
         (w (first (send self :size))))
    (when graph
          (send graph :draw-text (send self :title) loc-x (+ loc-y 15) 0 0)
          (send graph :frame-rect loc-x (+ loc-y 20) w 10)
          (send self :draw-indicator))))

(defmeth slider-control-proto :draw-indicator (&optional index)
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x (first loc))
         (loc-y (second loc))
         (w (first (send self :size)))
         (min (send self :min))
         (max (send self :max))
         (index (if index index (send self :index)))
         (val (floor (* (- w 7) (/ (- index min) (- max min))))))
    (when graph
          (let ((tw (send graph :text-width (send self :title))))
            (send graph :start-buffering)
            (send graph :erase-rect (+ 1 tw loc-x) loc-y (- w tw) 20)
            (send graph :draw-text 
                  (format nil "~a" (elt (send self :display) index))
                  (+ loc-x w) (+ loc-y 15) 2 0)
            (send graph :buffer-to-screen (+ 1 tw loc-x) loc-y (- w tw) 20))
          (send graph :erase-rect (+ 1 loc-x) (+ 21 loc-y) (- w 2) 8)
          (send graph :paint-rect (+ 1 loc-x val) (+ 21 loc-y) 5 8))))

(defmeth slider-control-proto :min () 0)

(defmeth slider-control-proto :max () (- (length (slot-value 'sequence)) 1))

(defmeth slider-control-proto :sequence (&optional (seq nil set) &key 
                                                   (display seq))
  (when set
        (setf (slot-value 'sequence) (coerce seq 'vector))
        (setf (slot-value 'display) (coerce display 'vector)))
  (slot-value 'sequence))

(defmeth slider-control-proto :display () (slot-value 'display))

(defmeth slider-control-proto :index (&optional (new nil set))
  (if set
      (let* ((new (max (send self :min) (min new (send self :max)))))
        (setf (slot-value 'index) new)
        (send self :draw-indicator)
        (send self :do-action (elt (send self :sequence) new))))
  (slot-value 'index))

(defmeth slider-control-proto :do-click (x y a b)
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x (nth 0 loc))
         (loc-y (nth 1 loc))
         (w (first (send self :size))))
    (when (and (< loc-x x (+ loc-x w)) (< (+ loc-y 20) y (+ loc-y 30)))
          (let ((pos (+ (floor (* (- w 7) (/ (send self :index) 
                                             (send self :max))))
                        loc-x)))
            (cond
              ((<= pos x (+ pos 5))
               (let ((off (- x pos)))
                 (send graph :while-button-down
                       #'(lambda (x y)
                           (let ((val (max (+ loc-x 1)
                                           (min (- x off) 
                                                (+ loc-x (- w 6))))))
                             (setf pos val)
                             (send self :draw-indicator 
                                   (floor (* (send self :max) 
                                             (/ (- pos loc-x) (- w 7)))))))))
                 (send self :index 
                       (floor (* (send self :max) 
                                 (/ (- pos loc-x) (- w 7))))))
              ((< loc-x x pos)
               (send graph :while-button-down
                     #'(lambda (x y)
                         (let ((pos (+ (floor (* w (/ (send self :index) 
                                                      (send self :max))))
                                       loc-x)))
                           (if (< x pos)
                               (send self :index (- (send self :index) 1)))
			   (pause 2)))
                     nil))
              ((< pos x (+ loc-x w))
               (send graph :while-button-down
                     #'(lambda (x y)
                         (let ((pos (+ (floor (* w (/ (send self :index) 
                                                      (send self :max))))
                                       loc-x)))
                           (if (> x pos)
                               (send self :index (+ (send self :index) 1)))
			   (pause 2)))
                     nil))))
          t)))

;;;;
;;;; Rotation example
;;;;

;;; Rotation around axes

(defproto spin-rotate-control-proto '(v) () rocker-control-proto)

(defmeth spin-rotate-control-proto :isnew (v)
  (call-next-method :v v :location (list 10 (case v (0 10) (1 30) (2 50)))))

(defmeth spin-rotate-control-proto :title ()
  (send (send self :graph) :variable-label (slot-value 'v)))

(defmeth spin-rotate-control-proto :do-action (first sign)
  (let ((graph (send self :graph)))
    (if first
        (let* ((v (slot-value 'v))
               (v1 (if (= v 0) 1 0))
               (v2 (if (= v 2) 1 2))
               (trans (send graph :transformation))
               (cols (column-list 
                      (if trans 
                          trans 
                          (identity-matrix (send graph :num-variables)))))
               (angle (send graph :angle)))
          (send graph :idle-on (car first))
          (send graph :slot-value 'rotation-type
                (make-rotation (nth v1 cols) (nth v2 cols) 
                               (case sign (+ angle) (- (- angle)))))))
    (send graph :rotate)
    (pause 2)))

;;; Plot Rocking Control

(defproto spin-rock-control-proto '(v) () graph-control-proto)

(defmeth spin-rock-control-proto :isnew ()
  (call-next-method :location '(10 70) :title "Rock Plot"))

(defmeth spin-rock-control-proto :do-action (first) 
  (send (send self :graph) :rock-plot))

(defmeth spin-proto :rock-plot (&optional (k 2))
  (let ((angle (send self :angle)))
    (dotimes (i k) (send self :rotate-2 0 2 angle)(pause 2))
    (dotimes (i (* 2 k)) (send self :rotate-2 0 2 (- angle)) (pause 2))
    (dotimes (i k) (send self :rotate-2 0 2 angle))) (pause 2))

;;;; Speed Control

(defproto spin-speed-control-proto () () slider-control-proto)

(defmeth spin-speed-control-proto :isnew (&optional (points 21))
  (call-next-method (rseq 0 .2 points) :location '(10 90) :title "Speed"))

(defmeth spin-speed-control-proto :do-action (v)
  (let ((graph (send self :graph)))
    (if graph (send graph :angle v))))

;;;; Installation method

(defmeth spin-proto :add-spin-controls ()
  (send self :margin 110 0 0 20)
  (apply #'send self :size (+ (send self :size) '(100 0)))
  (send self :resize)
  (send self :add-control (send spin-rotate-control-proto :new 0))
  (send self :add-control (send spin-rotate-control-proto :new 1))
  (send self :add-control (send spin-rotate-control-proto :new 2))
  (send self :add-control (send spin-rock-control-proto :new))
  (send self :add-control (send spin-speed-control-proto :new)))


        
