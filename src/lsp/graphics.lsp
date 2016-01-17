;;;;
;;;; graphics.lsp XLISP-STAT dynamic graphics functions and objects
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;

(provide "graphics")
(require "stats")
(require "dialogs")


;;;
;;; Constants
;;;

(defconstant *cursors* 
  '(arrow watch cross brush hand finger hour-glass trash-bag trash-can))
(defconstant *colors* 
  '(white black red green blue cyan magenta yellow))
(defconstant *plot-symbols* 
  '(dot dot1 dot2 dot3 dot4 disk diamond cross square wedge1 wedge2 x))


;;;
;;; Miscelaneous Functions
;;;

(defun close-all-plots ()
"Args: ()
Close all plot windos."
  (let ((plots (remove nil 
                       (mapcar #'(lambda (x) 
                                 (let ((object (nth 2 x)))
                                   (if (kind-of-p object graph-window-proto) 
                                       object
                                       nil)))
                               *hardware-objects*))))
    (mapcar #'(lambda (x) (send x :remove)) plots)))

(defun get-new-integer (prompt low high initial)
"Args: (prompt low high initial)
Open dialog to get an integer in the range (LOW HIGH). Returns the integer
or NIL if Cancelled or integer is not in range. Beeps if integer is bad."
  (let ((val (get-value-dialog (format nil "~a: (between ~d and ~d)" 
                                       prompt low high) :initial initial)))
    (if val (cond 
              ((and (integerp (car val)) (< low (car val) high)) (car val))
              (t (sysbeep) nil)))))

(defun linked-plots ()
"Args: ()
Return list of all linked plots."
  (let ((plots nil))
    (dolist (x *hardware-objects*)
            (let ((x (nth 2 x)))
              (if (and (kind-of-p x graph-proto) (send x :linked)) 
                  (push x plots))))
    plots))

(defun active-graph-windows ()
"Args: ()
Return list of all currently visible graph windows."
  (remove-if-not #'(lambda (x) (kind-of-p x graph-window-proto))
		 (mapcar #'third *hardware-objects*)))

(defun color-symbols ()
"Args: ()
Returns list of symbols with color definitions."
  (let ((res nil))
    (do-all-symbols (s res)
      (if (get s '|color-index|) (push s res)))))

(defun cursor-symbols ()
"Args: ()
Returns list of symbols with cursor definitions."
  (let ((res nil))
    (do-all-symbols (s res)
      (if (get s '|cursor-index|) (push s res)))))
      
(defun plot-symbol-symbols ()
"Args: ()
Returns list of symbols with plot symbol definitions."
  (let ((res nil))
    (do-all-symbols (s res)
      (if (get s '|symbol-index|) (push s res)))))

(defun pause (n)
"Args: (n)
Pause for N/60 seconds."
  (flush-graphics)
  (let ((ticks (+ (* (/ n 60) internal-time-units-per-second)
                  (get-internal-run-time))))
    (loop (if (<= ticks (get-internal-run-time)) (return)))))

;;;
;;; Functions to link several plots
;;;

(defun link-views (&rest args)
"Args: (&rest plots)
Links the argument plots: any change in hiliting or visibility of points in
the current plot is propagated to the other plots."
  (mapc #'(lambda (x) (send x :linked t)) args))
        
(defun unlink-views (&rest args)
"Args: (&rest plots)
Removes links to its arguments. With no arguments removes all links."
  (if args
      (mapc #'(lambda (x) (send x :linked nil)) args)
      (unlink-all-windows)))

;;;;
;;;; GRAPH-PROTO Methods
;;;;

(defmeth graph-proto :current-variables (&optional x y &key (draw t))
"Method args: (&optional x y &key (draw t))
Sets or retrieves current variables."
  (when x
	    (send self :content-variables x y)
	    (if draw (send self :redraw)))
  (send self :content-variables))

(defmeth spin-proto :current-variables (&optional x y z &key (draw t))
"Method args: (&optional x y z &key (draw t))
Sets or retrieves current variables."
  (when x
	    (send self :content-variables x y z)
	    (if draw (send self :redraw)))
  (send self :content-variables))

(defmeth graph-proto :scale-type (&optional (new nil set) &key (draw t))
"Method args: (&optional new)
Sets or returns scale type."
  (when set
;       (setf (slot-value 'scale-type) 
;		      (case new ((fixed variable) new) (t nil)))
        (setf (slot-value 'scale-type) new)
        (send self :adjust-to-data :draw draw))
  (slot-value 'scale-type))
  
(defmeth graph-proto :menu-title (&optional (title nil set))
  (if set (setf (slot-value 'menu-title) title))
  (slot-value 'menu-title))
  
(defmeth graph-proto :variable-labels (&optional (val nil set))
  (if set (setf (slot-value 'variable-labels) (map 'vector #'string val)))
  (slot-value 'variable-labels))
  
(defmeth graph-proto :black-on-white (&optional (val nil set))
  (if set (setf (slot-value 'black-on-white) val))
  (slot-value 'black-on-white))
  
(defmeth graph-proto :center (&rest args)
"Method args: (var &optional shift &key (draw t))
Sets or retrieves current center for variable VAR. Sends :RESIZE and :REDRAW
messages if DRAW is true. Vectorized."
  (apply #'send self :shift args))

; dummy message to allow depth cuing to be turned off when symbols
; are changed
(defmeth graph-proto :depth-cuing (&optional (on nil set))
  (declare (ignore set))
  nil)

(defmeth graph-proto :set-selection-symbol ()
"Method args: ()
Open dialog to set symbol used to plot selected points."
  (let* ((symbols (remove-if #'(lambda (x) (member x '(dot1 dot2 dot3 dot4))) 
                             *plot-symbols*))
         (i (choose-item-dialog "Symbol for selected points" 
                                (mapcar #'string symbols))))
    (when i
          (let ((plots (adjoin self (linked-plots)))
                (selection (send self :selection))
                (symbol (nth i symbols)))
            (dolist (p plots)
                    (if (send p :depth-cuing) (send p :depth-cuing nil))
                    (send p :point-symbol selection symbol)
                    (send p :redraw))))))

(defmeth graph-proto :set-selection-color ()
"Method args: ()
Open dialog to set color used to plot selected points."
  (let ((c (choose-item-dialog "Color for selected points" 
                                  (cons "None" (mapcar #'string *colors*)))))
    (when c
          (setf c (if (= c 0) nil (nth (- c 1) *colors*)))
          (let ((plots (adjoin self (linked-plots)))
                (selection (send self :selection)))
            (dolist (p plots)
                    (if c (send p :use-color t))
                    (send p :point-color selection c)
                    (send p :redraw))))))

(defmeth graph-proto :show-all-points ()
  (send self :point-showing (iseq 0 (- (send self :num-points) 1)) t)
  (send self :adjust-to-data))

(defmeth graph-proto :selection-dialog ()
"Method args: ()
Open dialog to save or set selection."
  (if (send self :any-points-selected-p)
      (let ((s (get-string-dialog "Name for the selection:")))
        (when s
              (setq s (intern (string-upcase s)))
              (set s (send self :selection))
              (pushnew s *variables*)))
      (let ((s (get-value-dialog "Expression for indices to select:")))
        (if s (send self :point-selected (car s) t)))))
              
;;; :DRAG-POINT - if there is a point close to the mouse drag it
;;; and return its index. Otherwise returns NIL.
;;; For transformed data result only makes sense if transform is
;;; orthogonal.
;;;
(defmeth graph-proto :drag-point (x y &key (draw t))
  (let* ((cont-vars (send self :content-variables))
         (cont-x (nth 0 cont-vars))
         (cont-y (nth 1 cont-vars))
         (tol 5)
         (width (* 2 tol))
         (points (send self :points-in-rect
                       (- x tol) (- y tol) width width))
         (p (if points (car points)))
         (transform (send self :transformation)))
    (if (null transform)
        (if p
            (let* ((coords (send self :drag-grey-rect x y tol tol))
                   (sx (+ (nth 0 coords) tol))
                   (sy (+ (nth 1 coords) tol))
                   (rcoords (send self :canvas-to-real sx sy)))
              (send self :point-coordinate cont-x p (nth 0 rcoords))
              (send self :point-coordinate cont-y p (nth 1 rcoords))
              (if draw (send self :redraw))
              p))
        (if p
            (let* ((coords (send self :drag-grey-rect x y tol tol))
                   (sx (+ (nth 0 coords) tol))
                   (sy (+ (nth 1 coords) tol))
                   (sc-coords (send self :canvas-to-scaled sx sy))
                   (sc-x (first sc-coords))
                   (sc-y (second sc-coords))
                   (old-sc-x (send self :point-transformed-coordinate 
                                   cont-x p))
                   (old-sc-y (send self :point-transformed-coordinate
                                   cont-y p))
                   (dims (iseq 0 (- (send self :num-variables) 1)))
                   (reals (send self :point-coordinate dims p))
                   (scale (send self :scale dims))
                   (x-col (compound-data-seq
                           (select transform cont-x dims)))
                   (y-col (compound-data-seq
                           (select transform cont-y dims))))
              (send self :point-coordinate dims p
                    (+ reals (* (+ (* x-col (- sc-x old-sc-x))
                                   (* y-col (- sc-y old-sc-y)))
                                scale)))
              (if draw (send self :redraw))
              p)))))          
          

;;; :SAVE-IMAGE - saves a postscript image to a file.
#+unix
(defmeth graph-window-proto :save-image (&optional (file "image.ps") &key (scale 1))
"Method args: (&optional (file \"image.ps\"))
Saves a postscript image of the window to file named FILE by buffering,
sending the window the :REDRAW message, and dumping the buffer to a file."
  (let ((file (open file :direction :output)))
    (unwind-protect (send self :image-to-file file scale)
      (close file))))

#+unix
(defmeth graph-window-proto :ask-save-image (&optional (file "image.ps"))
  (let ((file (get-string-dialog "Enter a file name for postscript output"
                                 :initial file)))
    (when file
        (if (= (length file) 0) (error "null file name string"))
        (format t "; saving postscript image to file ~a..." file)
        (force-output)
        (send self :save-image file)
        (format t "done~%"))))

;;;;
;;;;
;;;; Graph Object Menu and Dialog Methods
;;;;
;;;;

(send graph-proto :menu-title "Plot")

(defmeth graph-proto :menu-template (&optional (val nil set))
  (if set (setf (slot-value 'menu-template) val))
  (slot-value 'menu-template))

(send graph-proto :menu-template '(link showing-labels mouse resize-brush dash 
                                        erase-selection focus-on-selection
                                        show-all symbol
					color
                                        selection dash 
					slicer 
                                        rescale 
                                        options
#+unix                                  save-image))
         
(defmeth graph-proto :make-menu-item (item-template)
  (if (kind-of-p item-template menu-item-proto)
      item-template
      (case item-template
        (dash (send dash-item-proto :new))
        (link (send link-item-proto :new self))
        (erase-selection
         (send graph-item-proto :new "Remove Selection" self 
               :erase-selection :any-points-selected-p))
        (focus-on-selection
         (send graph-item-proto :new "Focus on Selection" self 
               :focus-on-selection :any-points-selected-p))
        (showing-labels 
         (send graph-item-proto :new "Show Labels" self
               :showing-labels :showing-labels :toggle t))
        (show-all
         (send graph-item-proto :new "Show All" self 
               :show-all-points :all-points-showing-p :negate t))
        (selection
         (send graph-item-proto :new "Selection ..." self 
               :selection-dialog))
        (mouse (send mouse-mode-item-proto :new self))
        (resize-brush 
         (send graph-item-proto :new "Resize Brush" self :resize-brush))
        (redraw 
         (send graph-item-proto :new "Redraw Plot" self :redraw))
        (rescale 
         (send graph-item-proto :new "Rescale Plot" self :adjust-to-data))
        (options 
	 (if (not (small-machine-p))
	     (send graph-item-proto :new "Options ..." self :set-options)))
        (slicer
	 (if (not (small-machine-p))
	     (send graph-item-proto :new
		   "Slicer ..." self :make-slicer-dialog)))
        (symbol
         (send graph-item-proto :new "Selection Symbol" self
               :set-selection-symbol :any-points-selected-p))
        (color
	 (if (screen-has-color)
	     (send graph-item-proto :new "Selection Color" self
		   :set-selection-color :any-points-selected-p)))
        (save-image
         (send graph-item-proto :new "Save to File" self :ask-save-image)))))

(defmeth graph-proto :new-menu (&optional title &key (items (send self :menu-template)))
  (unless title (setq title (slot-value 'menu-title)))
  (if (slot-value 'menu) (send (slot-value 'menu) :dispose))
  (flet ((make-item (item) (send self :make-menu-item item)))
    (let ((menu (send menu-proto :new title)))
      (send self :menu menu)
      (apply #'send menu :append-items  (remove nil (mapcar #'make-item items)))
      menu)))
    
(defmeth graph-window-proto :clobber ()
  (call-next-method)
  (if (slot-value 'menu) (send (slot-value 'menu) :dispose)))

(defmeth graph-window-proto :close () (send self :remove))

(defmeth graph-proto :remove ()
  (if (send self :allocated-p) (send self :linked nil))
  (call-next-method))

(defmeth graph-proto :add-dialog (d) (send self :add-subordinate d))
(defmeth graph-proto :remove-dialog (d) (send self :delete-subordinate d))

(defmeth graph-proto :clear (&rest args)
"Message args: (&key (draw t))
Clears the plot data. If DRAW is nil the plot is redrawn; otherwise its
current screen image remains unchanged."
  (let ((draw (null args)))       ;;;temporary cheat to allow optional or key arg
    (send self :clear-points :draw nil)
    (send self :clear-lines :draw draw)))

(defmeth graph-proto :focus-on-selection ()
"Message args: ()
removes all unselected points invisible"
  (let ((i (iseq 0 (- (send self :num-points) 1))))
    (send self :point-showing (set-difference i (send self :selection)) nil)))

;;
;; Graph Menu Item Prototypes
;;

;; GRAPH-ITEM-PROTO
(defproto graph-item-proto 
          '(graph update-message toggle negate redraw) () menu-item-proto)

(defmeth graph-item-proto :isnew (title graph message 
           &optional update-message &key toggle negate redraw)
  (setf (slot-value 'graph) graph)
  (setf (slot-value 'action) message)
  (setf (slot-value 'update-message) update-message)
  (setf (slot-value 'toggle) toggle)
  (setf (slot-value 'negate) negate)
  (setf (slot-value 'redraw) redraw)
  (call-next-method title))
  
(defmeth graph-item-proto :do-action ()
  (let ((graph (slot-value 'graph))
        (action (send self :action)))
    (if (and action (symbolp action)) 
        (if (slot-value 'toggle) 
            (send graph action (not (send self :update-message)))
            (send graph action))
        (if action (funcall action graph)))
    (if (slot-value 'redraw) (send graph :redraw))))

(defmeth graph-item-proto :update-message ()
  (let ((graph (slot-value 'graph)))
    (if (slot-value 'update-message)
        (if (symbolp (slot-value 'update-message))
            (send graph (slot-value 'update-message))
            (funcall (slot-value 'update-message) graph)))))

(defmeth graph-item-proto :update () 
  (if (slot-value 'update-message)
    (let ((test (if (send self :update-message) t nil)))
      (send self 
           (if (slot-value 'toggle) :mark :enabled) 
           (if (slot-value 'negate) (not test) test)))))

;; LINK-ITEM-PROTO. Toggles linking and menu text 
(defproto link-item-proto '(graph) () menu-item-proto)

(defmeth link-item-proto :isnew (v)
  (setf (slot-value 'graph) v)
  (call-next-method "Link View"))
  
(defmeth link-item-proto :do-action ()
  (send (slot-value 'graph) 
        :linked (not (send (slot-value 'graph) :linked))))
  
(defmeth link-item-proto :update () 
  (send self :title 
        (if (send (slot-value 'graph) :linked) 
            "Unlink View"
            "Link View")))

;; MOUSE-MODE-ITEM-PROTO. Rotates among selecting, brushing, user modes

(defproto mouse-mode-item-proto '(graph) () menu-item-proto
  "Menu item for changing the mouse mode")

(defmeth mouse-mode-item-proto :isnew (v)
  (setf (slot-value 'graph) v)
  (call-next-method "Mouse Mode ..."))

(defmeth mouse-mode-item-proto :do-action ()
  (let ((graph (slot-value 'graph)))
    (send graph :choose-mouse-mode)))

;;;
;;; Graph mode methods
;;;

(defmeth graph-proto :add-mouse-mode (mode &key 
                                           (title (string mode))
                                           (cursor 'arrow)
                                           click
					   motion)
"Method args: (mode &key (title (string mode)) (cursor 'arrow) click motion)
Adds MODE to the mode list. TITLE is the string used in the menu, CLICK
and MOTION are the selectors for the mouse actions."
  (let ((mode-list (remove mode (slot-value 'mode-list)
                           :test #'(lambda (x y) 
                                     (and (consp y) (eql x (car y)))))))
    (send self :add-slot 'mode-list
          (cons (list mode title cursor click motion) mode-list)))
   mode)

(defmeth graph-proto :delete-mouse-mode (mode)
"Method args: (mode)
Deletes MODE to the mode list."
  (let* ((mode-list (slot-value 'mode-list))
         (new-list (remove mode mode-list
                           :test #'(lambda (x y) 
                                     (and (consp y) (eql x (car y)))))))
    (unless (equal mode-list new-list)
            (if (null new-list) (error "Need at least one mouse mode"))
            (send self :add-slot 'mode-list new-list)
            (if (eq mode (send self :mouse-mode))
                (send self :mouse-mode (first (send self :mouse-modes))))
            mode)))

(defmeth graph-proto :set-mode-cursor ()
"Method args: ()
Installs appropriate cursor for current mode."
  (let* ((mode-info (assoc (slot-value 'mouse-mode) (slot-value 'mode-list)))
         (cursor (if mode-info (nth 2 mode-info) 'arrow)))
    (send self :cursor cursor)))
    
(send graph-proto :add-mouse-mode 'selecting 
      :title "Selecting Mode" :click :do-select-click)
(send graph-proto :add-mouse-mode 'brushing 
      :title "Brushing Mode" :cursor 'brush 
      :click :do-brush-click :motion :do-brush-motion)

(defmeth graph-proto :mouse-mode-title (mode)
"Method args: (mode)
Returns title for menu item for mouse mode MODE."
  (let ((mode-info (assoc mode (slot-value 'mode-list))))
    (if mode-info (nth 1 mode-info))))

(defmeth graph-proto :mouse-modes ()
"Method args: ()
Returns a list of the current mouse modes."
  (reverse (mapcar #'car (slot-value 'mode-list))))

(defmeth graph-proto :choose-mouse-mode ()
"Method args: ()
Presents a dialog to set the mouse mode."
  (let* ((modes (send self :mouse-modes))
         (m (choose-item-dialog "New Mode:"
                                (mapcar #'(lambda (x) 
                                            (send self :mouse-mode-title x))
                                        modes)
                                :initial (position (send self :mouse-mode)
                                                   modes))))
    (if m (send self :mouse-mode (nth m modes)))))

;;;
;;; Overlay Methods
;;;

(defmeth graph-proto :redraw-overlays ()
  (dolist (ov (reverse (slot-value 'overlays)))
          (send ov :redraw)))
  
(defmeth graph-proto :resize-overlays ()
  (dolist (ov (slot-value 'overlays))
          (send ov :resize)))
  
(defmeth graph-proto :overlay-click (x y m1 m2)
  (dolist (ov (slot-value 'overlays))
    (if (send ov :do-click x y m1 m2) (return t))))

(defmeth graph-proto :add-overlay (ov)
  (if (send ov :graph) (error "Already installed in a graph"))
  (send ov :slot-value 'graph self)
  (setf (slot-value 'overlays) (cons ov (slot-value 'overlays))))

(defmeth graph-proto :delete-overlay (ov)
  (when (member ov (slot-value 'overlays))
        (send ov :slot-value 'graph nil)
        (setf (slot-value 'overlays) (remove ov (slot-value 'overlays)))))

(defproto graph-overlay-proto '(graph))

(defmeth graph-overlay-proto :graph () (slot-value 'graph))
(defmeth graph-overlay-proto :resize () nil)
(defmeth graph-overlay-proto :redraw () nil)
(defmeth graph-overlay-proto :do-click (x y m1 m2)
  (declare (ignore x y m1 m2))
  nil)

(require "graph2")
