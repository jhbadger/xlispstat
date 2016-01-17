(provide "graph3")
(require "graphics")

;;;
;;; Options dialog stuff
;;;

(defproto graph-toggle-item-proto '(graph message) () toggle-item-proto)

(defmeth graph-toggle-item-proto :isnew (title graph message)
  (setf (slot-value 'graph) graph)
  (setf (slot-value 'message) message)
  (call-next-method title :value (send graph message)))

(defmeth graph-toggle-item-proto :set-value ()
  (let* ((message (slot-value 'message))
         (graph (slot-value 'graph))
         (old (if (send graph message) t nil))
         (new (if (send self :value) t nil)))
    (unless (eq old new) (send graph message new))))

(defproto graph-backcolor-choice-item-proto '(graph) () choice-item-proto)

(defmeth graph-backcolor-choice-item-proto :isnew (graph)
  (setf (slot-value 'graph) graph)
  (call-next-method (list "White Background" "Black Background") 
                    :value (if (eq (send graph :back-color) 'white) 0 1)))

(defmeth graph-backcolor-choice-item-proto :set-value ()
  (let ((graph (slot-value 'graph)))
    (case (send self :value)
      (0 (send graph :back-color 'white)
         (send graph :draw-color 'black))
      (1 (send graph :back-color 'black)
         (send graph :draw-color 'white)))))

(defproto graph-scaling-choice-item-proto '(graph) () choice-item-proto)

(defmeth graph-scaling-choice-item-proto :isnew (graph)
  (setf (slot-value 'graph) graph)
  (call-next-method (list "Variable Scaling" "Fixed Scaling" "No Scaling")
                    :value (case (send graph :scale-type) 
                                 (variable 0)
                                 (fixed 1) 
                                 (t 2))))

(defmeth graph-scaling-choice-item-proto :set-value ()
  (let ((graph (slot-value 'graph)))
    (send graph :scale-type
          (case (send self :value)
                (0 'variable)
                (1 'fixed)
                (2 nil)))))

(defmeth graph-proto :set-options ()
"Method args: ()
Opens dialog to set plot options. Items are obtained using the
:make-options-dialog-items message."
  (let* ((items (send self :make-options-dialog-items))
         (d (send ok-or-cancel-dialog-proto :new items :title "Options"
                  :ok-action #'(lambda ()
                                 (dolist (i items) 
                                         (send i :set-value))
                                 (send self :redraw)))))
    (unwind-protect (send d :modal-dialog)
                    (send d :remove))))

(defmeth graph-proto :make-options-dialog-items ()
  (remove
   nil
   (list
    (send graph-backcolor-choice-item-proto :new self)
    (send graph-toggle-item-proto :new "Vertical Scroll" self :has-v-scroll)
    (send graph-toggle-item-proto :new "Horizontal Scroll" self :has-h-scroll)
    (send graph-toggle-item-proto :new "Fixed Aspect Ratio" self :fixed-aspect)
    (if (screen-has-color)
	(send graph-toggle-item-proto :new "Use color" self :use-color)))))

(defmeth scatmat-proto :make-options-dialog-items ()
  (remove
   nil
   (list
    (send graph-backcolor-choice-item-proto :new self)
    (send graph-toggle-item-proto :new "Vertical Scroll" self :has-v-scroll)
    (send graph-toggle-item-proto :new "Horizontal Scroll" self :has-h-scroll)
    (if (screen-has-color)
	(send graph-toggle-item-proto :new "Use color" self :use-color)))))

(defmeth spin-proto :make-options-dialog-items ()
  (remove
   nil
   (list 
    (send graph-backcolor-choice-item-proto :new self)
    (send graph-scaling-choice-item-proto :new self)
    (if (screen-has-color)
	(send graph-toggle-item-proto :new "Use color" self :use-color)))))

;;;;
;;;;
;;;; Plot Sliders and Slicers
;;;;
;;;;

;;; Graph dialogs

(defproto graph-dialog-proto '(plot))

(defmeth graph-dialog-proto :install (plot)
  (setf (slot-value 'plot) plot)
  (send plot :add-subordinate self))

(defmeth graph-dialog-proto :clobber ()
  (let ((plot (slot-value 'plot)))
    (if plot (send plot :delete-subordinate self)))
  (setf (slot-value 'plot) nil))

;;; Graph slicers

(defmeth graph-proto :add-slicer (s)
  (setf (slot-value 'slicers) (adjoin s (slot-value 'slicers)))
  (if (send self :allocated-p) (send self :adjust-slices)))
  
(defmeth graph-proto :remove-slicer (s)
  (setf (slot-value 'slicers) (remove s (slot-value 'slicers)))
  (when (send self :allocated-p)
        (if (eq 'show (send s :type)) (send self :show-all-points))
        (send self :adjust-slices)))

(defproto graph-slicer-proto
          '(variable delta selecting)
          () 
          (list graph-dialog-proto interval-slider-dialog-proto))

(defmeth graph-slicer-proto :isnew (plot var delta range
                                         &rest args
                                         &key select)
  (setf (slot-value 'variable) var)
  (setf (slot-value 'delta) delta)
  (setf (slot-value 'selecting) select)
  (apply #'call-next-method range 
         :action #'(lambda (x) (send plot :adjust-slices)) args)
  (send self :install plot))

(defmeth graph-slicer-proto :install (plot)
  (call-next-method plot)
  (send plot :add-slicer self))
  
(defmeth graph-slicer-proto :clobber ()
  (let ((plot (slot-value 'plot)))
    (if plot (send plot :remove-slicer self)))
  (call-next-method))

(defmeth graph-slicer-proto :selection ()
  (let ((x (send self :value))
        (var (slot-value 'variable))
        (d (slot-value 'delta)))
    (which (< (- x d) var (+ x d)))))

(defmeth graph-slicer-proto :type ()
  (if (slot-value 'selecting) 'select 'show))
  
(defmeth graph-proto :adjust-slices ()
  (cond
    ((slot-value 'slicers)
     (let ((indices (reduce #'intersection 
                            (mapcar #'(lambda (x) (send x :selection))
                                    (slot-value 'slicers))))
            (show (some #'(lambda (x) (eq 'show (send x :type))) 
                        (slot-value 'slicers))))
       (cond
         (show (send self :points-showing indices))
         (t (send self :points-selected indices)))))
    (t (send self :unselect-all-points) (send self :show-all-points))))   

;; Installing graph slicers

(defmeth graph-proto :slicer (var &rest args 
                                  &key 
                                  (fraction 0.25)
                                  title
                                  (points 20))
  (unless title (setq title "Slicer"))
  (let* ((range (list (min var) (max var)))
         (p (* 0.5  fraction (- (nth 1 range) (nth 0 range))))
         (slicer (apply #'send graph-slicer-proto :new self var p
                        (list (+ (nth 0 range) p) (- (nth 1 range) p))
                        :title title
                        :points points
                        args)))
    (send slicer :value (/ (+ (nth 0 range) (nth 1 range)) 2))
    slicer))
    
(defmeth graph-proto :make-slicer-dialog ()
  (let* ((fractions (list 0.1 0.2 0.3))
         (var-item (send edit-text-item-proto :new 
                         (format nil "(iseq 0 ~d)          " 
                                 (- (send self :num-points) 1))))
         (fraction-item (send choice-item-proto :new 
                              (mapcar #'(lambda (x) (format nil "~a" x))
                                      fractions) 
                              :value 1))
         (type-item (send choice-item-proto :new 
                          (list "Select Slice"
                                "Show Only Slice")))
         expr
         title
         var
         fraction
         select
         ok)
    (flet ((ok-action ()
                      (setq expr (read (make-string-input-stream 
                                        (send var-item :text))))
                      (setq title (format nil "~a" expr))
                      (setq var (eval expr))
                      (setq fraction (nth (send fraction-item :value)
                                          fractions))
                      (setq select (= 0 (send type-item :value)))
                      t))
      (let* ((d (send ok-or-cancel-dialog-proto :new 
                      (list (send text-item-proto :new "Variable")
                            var-item
                            (list (list 
                                   (send text-item-proto :new "Fraction")
                                   fraction-item)
                                  (list 
                                   (send text-item-proto :new "Slicer Type")
                                   type-item)))
                      :ok-action #'ok-action)))
        (unwind-protect (setq ok (send d :modal-dialog))
                        (send d :remove))))
    (if ok 
        (send self :slicer var 
              :title title 
              :fraction fraction 
              :select select))))
              
	    
