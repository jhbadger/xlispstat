#+macintosh(require "addbox" ":Examples:addbox")
#+macintosh(require "addhandrotate" ":Examples:addhandrotate")
#+macintosh(require ":Data:tutorial")
#+unix(require "addbox" "Examples/addbox")
#+unix(require "addhandrotate" "Examples/addhandrotate")
#+unix(load-data "tutorial")
#+msdos(require "addbox" "Examples\\addbox")
#+msdos(require "addhandrotate" "Examples\\addhandr")
#+msdos(load-data "tutorial")

(defun sphere-rand (n &optional (dim 3))
  (mapcar #'(lambda (k) 
              (do ((x 
                    (- (* 2 (uniform-rand k)) 1) 
                    (- (* 2 (uniform-rand k)) 1)))
                  ((< (sum (* x x)) 1) x))) (repeat dim n)))

(defmeth spin-proto :data-rotate (&optional axis (angle pi))
  (unless axis
          (setf axis (choose-item-dialog "Axis:" '("X" "Y" "Z")))
          (if axis (setf axis (select '(x y z) axis)))
          (send self :redraw))
  (if axis
      (let* ((alpha (send self :angle))
             (cols (column-list 
                    (let ((m (send self :transformation)))
                       (if m m (identity-matrix 3)))))
             (m (case axis
                  (x (make-rotation (nth 1 cols) (nth 2 cols) alpha))
                  (y (make-rotation (nth 0 cols) (nth 2 cols) alpha))
                  (z (make-rotation (nth 0 cols) (nth 1 cols) alpha)))))
        (dotimes (i (floor (/ angle alpha)))
                 (send self :apply-transformation m)))))

(defmeth spin-proto :toggle-box ()
  (if (not (send self :has-slot 'has-box :own t))
      (send self :add-slot 'has-box))
  (let ((has-box (slot-value 'has-box)))
    (if (not has-box) (send self :add-box) (send self :clear-lines :draw nil))
    (send self :redraw)
    (setf (slot-value 'has-box) (not has-box))))

(defmeth spin-proto :rock-plot (&optional (n 10) (k 3))
  (let ((a (send self :angle)))
    (dotimes (i k) (send self :rotate-2 0 2 (- a)))
    (dotimes (i n)
             (dotimes (i (* 2 k)) (send self :rotate-2 0 2 a))
             (dotimes (i (* 2 k)) (send self :rotate-2 0 2 (- a))))))

(defun add-demo-menu-items (bar)
  (send (send bar :menu) :append-items 
        (send dash-item-proto :new)
        (send menu-item-proto :new "Toggle Box" :action 
              #'(lambda () (send bar :toggle-box)))
        (send menu-item-proto :new "Toggle Scaling" :action
              #'(lambda ()
                  (send bar :scale-type
                        (if (eq (send bar :scale-type) 'fixed)
                            'variable
                            'fixed))))
        (send menu-item-proto :new "Rotate..." :action
              #'(lambda () (send bar :data-rotate)))
        (send menu-item-proto :new "Rock Plot" :key #\R :action
              #'(lambda () (send bar :rock-plot)))))

(defun make-bar-demo ()
  (close-all-plots)
  (def bar (spin-plot (let* ((x1 (* 20 (uniform-rand 40)))
                             (x2 (normal-rand 40))
                             (y (normal-rand 40)))
                        (list x1 y x2))
                      :variable-labels '("X1" "Y" "X2")
                      :scale 'fixed))
  (send bar :depth-cuing nil)
  (send bar :redraw)
  (add-demo-menu-items bar))

(defun make-abrasion-demo ()
  (close-all-plots)
  (def abr (spin-plot (list tensile-strength abrasion-loss hardness) 
                      :variable-labels '("T" "A" "H")))
  (add-demo-menu-items abr))

(defun make-spheres-demo ()
  (close-all-plots)
  (let ((x (sphere-rand 100)))
    (def p1 (spin-plot (transpose x)))
    (add-demo-menu-items p1)
    (def p2 (spin-plot (transpose (mapcar 
                                   #'(lambda (x) 
                                       (let ((n (sqrt (sum (* x x))))) 
                                         (* (+ .8 (* .2 n)) (/ x n)))) x))))
    (send p2 :location 250 21) 
    (add-demo-menu-items p2)))

(defun make-randu-demo ()
  (close-all-plots)
  #+macintosh (require ":Data:randu")
  #+unix (load-data "randu")
  (let ((p (spin-plot randu))) (add-demo-menu-items p))
  (undef 'randu))

(defun make-diabetes-demo ()
  (close-all-plots)
  #+macintosh (require ":Data:diabetes")
  #+unix (load-data "diabetes")
  (let ((p (spin-plot (select diabetes '(0 1 2))
                      :variable-labels (select dlabs '(0 1 2)))))
    (add-demo-menu-items p))
  (undef 'diabetes))

(setf demo-menu (send menu-proto :new "Demos"))
(send demo-menu :append-items
      (send menu-item-proto :new "Bar" :action
            #'(lambda () (make-bar-demo)))
      (send menu-item-proto :new "Abrasion" :action
            #'(lambda () (make-abrasion-demo)))
      (send menu-item-proto :new "Spheres" :action
            #'(lambda () (make-spheres-demo)))
      (send menu-item-proto :new "Randu" :action
            #'(lambda () (make-randu-demo)))
      (send menu-item-proto :new "Diabetes" :action
            #'(lambda () (make-diabetes-demo))))
(send demo-menu :install)

(defun demo (which)
  (case which
	(bar (make-bar-demo))
	(abrasion (make-abrasion-demo))
	(spheres (make-spheres-demo))
	(randu (make-randu-demo))
	(diabetes (make-diabetes-demo))))

