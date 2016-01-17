;;;;
;;;; graphics.lsp XLISP-STAT custom dialog objects and functions
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;

(in-package "XLISP")
(provide "dialogs")
(export '(num-to-string ok-or-cancel-dialog-proto ok-or-cancel-dialog
          message-dialog-proto message-dialog
          get-string-dialog-proto get-string-dialog get-value-dialog
          choose-item-dialog-proto choose-item-dialog
          choose-subset-dialog-proto choose-subset-dialog
          sequence-scroll-item-proto sequence-slider-dialog-proto
		  sequence-slider-dialog 
          interval-scroll-item-proto interval-slider-dialog-proto
		  interval-slider-dialog))

(defun num-to-string (n) (prin1-to-string n))
		  
;;;;
;;;;
;;;; OK-or-Cancel Dialog Prototype
;;;;
;;;;

(defproto ok-or-cancel-dialog-proto 
  '(ok-button cancel-button) () modal-dialog-proto)

(defmeth ok-or-cancel-dialog-proto :isnew (items &rest args
                                                 &key (ok-default t) 
                                                 (ok-action #'(lambda () t))
                                                 (cancel-action 
                                                  #'(lambda () nil)))
  (let ((items (if (consp items) items (list items)))
        (ok-button (send modal-button-proto :new "OK" 
                         :action ok-action))
        (cancel-button (send modal-button-proto :new "Cancel"
                             :action cancel-action)))
    (setf items (mapcar #'(lambda (x) 
                                  (if (stringp x) 
                                      (send text-item-proto :new x)
                                      x))
                        items))
    (setf (slot-value 'ok-button) ok-button)
    (setf (slot-value 'cancel-button) cancel-button)
    (apply #'call-next-method
           (append items (list (list ok-button cancel-button)))
           args)
    (send self :default-button (if ok-default ok-button cancel-button))))
  
(defun ok-or-cancel-dialog (s &optional (ok-default t) &rest args)
"Args: (s &optional (ok-default t) &rest args)
Open modal dialog with string S and OK, Cancel buttons. Returns T for
OK, NIL for Cancel. S can contain format directives, which are filled
from the remaining arguments."
  (let ((d (send ok-or-cancel-dialog-proto :new 
                 (apply #'format nil s args) :ok-default ok-default)))
    (send d :modal-dialog)))

;;;;
;;;;
;;;; Message Dialog Prototype
;;;;
;;;;

(defproto message-dialog-proto '() () modal-dialog-proto)

(defmeth message-dialog-proto :isnew (s)
  (let ((text (if (consp s) s (list s)))
        (ok-button (send modal-button-proto :new "OK")))
    (call-next-method (append text (list ok-button)))
    (send self :default-button ok-button)))
  
(defun message-dialog (&rest args)
"Args: (s &rest args)
Open modal dialog with string S and OK buttons. Returns NIL. S can contain
format directives, which are filled from the remaining arguments."
  (let ((d (send message-dialog-proto :new (apply #'format nil args))))
     (send d :modal-dialog)))

;;;;
;;;;
;;;; Get String/Value Dialog Prototype
;;;;
;;;;

(defproto get-string-dialog-proto () () ok-or-cancel-dialog-proto)

(defmeth get-string-dialog-proto :isnew (s &rest args &key (initial nil has-init))
  (let* ((prompt-item (send text-item-proto :new s))
         (edit-item (send edit-text-item-proto :new 
                          (if has-init (format nil "~a" initial) "")
                          :text-length 20)))
    (apply #'call-next-method 
           (list prompt-item edit-item)
           :ok-action #'(lambda () (send edit-item :text))
           args)))

(defun get-string-dialog (&rest args)
"Args: (s &key initial)
Opens a modal dialog with prompt S, a text field and OK, Cancel buttons.
INITIAL is converted to a string with ~A format directive. Returns string
of text field content on OK, NIL on cancel."
  (let ((d (apply #'send get-string-dialog-proto :new args)))
    (send d :modal-dialog)))

(defun get-value-dialog (prompt &rest args &key (initial "" supplied))
"Args: (s &key initial)
Opens a modal dialog with prompt S, a text field and OK, Cancel buttons.
INITIAL is converted to a string with ~S format directive. On Cancel returns
NIL. ON OK Returns list of result of reading and eval'ing the text field's
content."
  (let* ((initial (if supplied
                      (format nil "~s" initial)
                      initial))
         (s (apply #'get-string-dialog prompt :initial initial args)))
    (if s (list (eval (read (make-string-input-stream s) nil))))))
  	
;;;;
;;;;
;;;; Choose string/value dialog prototype
;;;;
;;;;

(defproto choose-item-dialog-proto () () ok-or-cancel-dialog-proto)

(defmeth choose-item-dialog-proto :isnew (s strings &rest args 
                                            &key (initial 0))
  (let* ((prompt-item (send text-item-proto :new s))
         (string-item (send choice-item-proto :new strings :value initial)))
    (apply #'call-next-method (list prompt-item string-item)
           :ok-action #'(lambda () (send string-item :value))
           args)))

(defun choose-item-dialog (&rest args)
"Args: (s strings &key initial)
Opens modal dialog with prompt S, a choice item for list of strings STRINGS
and OK, Cancel buttons. Returns chosen string on OK, NIL on cancel."
  (let ((d (apply #'send choose-item-dialog-proto :new args)))
    (send d :modal-dialog)))

;;;;
;;;;
;;;; Choose string/value dialog prototype
;;;;
;;;;

(defproto choose-subset-dialog-proto () () ok-or-cancel-dialog-proto)

(defmeth choose-subset-dialog-proto :isnew (s strings &rest args
                                              &key (initial nil))
  (let ((prompt-item (send text-item-proto :new s))
        (subset-items (mapcar #'(lambda (x y) 
                                  (send toggle-item-proto
                                        :new x :value (member y initial)))
                              strings (iseq 0 (- (length strings) 1)))))
    (apply #'call-next-method (cons prompt-item subset-items)
           :ok-action #'(lambda () 
                          (list (which (mapcar #'(lambda (x) (send x :value))
                                               subset-items))))
           args)))

(defun choose-subset-dialog (&rest args)
"Args: (s strings &key initial)
Opens modal dialog with prompt S, a set of toggle items for list of 
strings STRINGS, and OK, Cancel buttons. Returns list of list of indices
of chosen items on OK, NIL on cancel."
  (let ((d (apply #'send choose-subset-dialog-proto :new args)))
    (send d :modal-dialog)))

;;;;
;;;;
;;;; Sequence Scroll Bar Item Prototype
;;;;
;;;;

(defproto sequence-scroll-item-proto 
  '(sequence display-sequence value-text-item) () scroll-item-proto)

(defmeth sequence-scroll-item-proto :isnew 
  (x &key text-item (size '(180 16)) location action display)
  (let* ((sequence (coerce x 'vector))
         (display (if display (coerce display 'vector) sequence)))
    (setf (slot-value 'sequence) sequence)
    (setf (slot-value 'display-sequence) display)
    (setf (slot-value 'value-text-item) text-item)
    (call-next-method :size size
                      :location location
                      :min-value 0 :max-value (1- (length sequence))
                      :page-increment 5
                      :action action)))
              
(defmeth sequence-scroll-item-proto :scroll-action ()
  (send self :display-value)
  (send self :user-action))

(defmeth sequence-scroll-item-proto :do-action ()
  (send self :display-value)
  (send self :user-action))

(defmeth sequence-scroll-item-proto :value (&optional (val nil set))
  (when set (call-next-method val) (send self :display-value))
  (call-next-method))

(defmeth sequence-scroll-item-proto :display-value ()
  (if (slot-value 'value-text-item) 
      (send (slot-value 'value-text-item) :text 
            (format nil "~s" 
                    (elt (slot-value 'display-sequence) 
                         (send self :value))))))

(defmeth sequence-scroll-item-proto :user-action ()
  (if (slot-value 'action)
      (funcall (slot-value 'action)
               (elt (slot-value 'sequence) (send self :value)))))
  
;;;;
;;;;
;;;; Sequence Slider Dialog Prototype
;;;;
;;;;

(defproto sequence-slider-dialog-proto () () dialog-proto)

(defmeth sequence-slider-dialog-proto :isnew 
  (data &key (text "Value") (title "Slider") action display)
  (let* ((name-item (send text-item-proto :new text))
         (value-item (send text-item-proto :new "          "
                           :location '(100 5)))
         (scroll-item (send sequence-scroll-item-proto :new data 
                            :text-item value-item
                            :action action :display display)))
    (call-next-method (list name-item value-item scroll-item) :title title)
    (send scroll-item :display-value)))

(defmeth sequence-slider-dialog-proto :value (&rest args)
  (apply #'send (nth 2 (slot-value 'items)) :value args))

(defun sequence-slider-dialog (&rest args)
"Args: (data &key (text \"Value\") (title \"Slider\") action display)
Opens modeless dialog with title TITLE, prompt TEXT, a text display and a
scrollbar. The scrollbar scrolls through the DATA sequence and displays the
corresponding element of the DISPLAY sequence. When a scroll event occurs
ACTION is called with the current value of DATA as argument."
  (apply #'send sequence-slider-dialog-proto :new args))


;;;;
;;;;
;;;; Interval Scroll Bar Item Prototype
;;;;
;;;;

(defproto interval-scroll-item-proto 
  '(interval num-points value-text-item) () scroll-item-proto)

(defmeth interval-scroll-item-proto :isnew 
  (x &key text-item (size '(180 16)) location action
          (points (nth 2 (get-nice-range (nth 0 x) (nth 1 x) 50))))
  (setf (slot-value 'interval) x)
  (setf (slot-value 'num-points) points)
  (setf (slot-value 'value-text-item) text-item)
  (call-next-method :size size :location location :min-value 0
                    :max-value (1- points)
                    :action action))
              
(defmeth interval-scroll-item-proto :value (&optional (val nil set))
  (let ((interval (slot-value 'interval))
        (num-points (slot-value 'num-points)))
    (if set 
        (let* ((min (elt interval 0))
               (max (elt interval 1))
               (val (floor (* (1- num-points) (/ (- val min) (- max min))))))
          (call-next-method val)
          (send self :display-value)
          (send self :user-action)))
    (let ((min (elt interval 0))
          (max (elt interval 1)))
      (+ min (* (/ (call-next-method) (1- num-points)) (- max min))))))

(defmeth interval-scroll-item-proto :max (&optional (max nil set))
  (let ((value (send self :value)))
    (when set (setf (elt interval 1) max) (send self :value value))
    (elt interval 1)))
    
(defmeth interval-scroll-item-proto :min (&optional (min nil set))
  (let ((value (send self :value)))
    (when set (setf (elt interval 0) min) (send self :value value))
    (elt interval 0)))

(defmeth interval-scroll-item-proto :user-action ()
  (if (slot-value 'action)
      (funcall (slot-value 'action) (send self :value))))
  
(defmeth interval-scroll-item-proto :display-value ()
  (if (slot-value 'value-text-item)
      (send (slot-value 'value-text-item)
            :text (num-to-string (send self :value)))))

(defmeth interval-scroll-item-proto :scroll-action ()
  (send self :display-value)
  (send self :user-action))

(defmeth interval-scroll-item-proto :do-action ()
  (send self :display-value)
  (send self :user-action))

;;;;
;;;;
;;;; Interval Slider Dialog Prototype
;;;;
;;;;

(defproto interval-slider-dialog-proto () () dialog-proto)

(defmeth interval-slider-dialog-proto :isnew 
  (data &key (text "Value") (title "Slider") action (points 30) (nice t))
  (if nice
      (let ((range (get-nice-range (nth 0 data) (nth 1 data) points)))
        (setq data (list (nth 0 range) (nth 1 range)))
        (setq points (nth 2 range))))
  (let* ((value-item (send text-item-proto :new "              "
                           :location '(100 5)))
         (name-item (send text-item-proto :new text))
         (scroll-item (send interval-scroll-item-proto :new data 
                            :text-item value-item
                            :action action :points points)))
    (call-next-method (list name-item value-item scroll-item) :title title)
    (send scroll-item :display-value)))

(defmeth interval-slider-dialog-proto :value (&rest args)
  (apply #'send (nth 2 (slot-value 'items)) :value args))

(defun interval-slider-dialog (&rest args)
"Args: (data &key (text \"Value\") (title \"Slider\") action (points 30) (nice t))
Opens modeless dialog with title TITLE, prompt TEXT, a text display and a
scrollbar. The scrollbar scrolls through the interval DATA, a list of the form
(LOW HIGH), sequence and displays the value. When a scroll event occurs
ACTION is called with the current value in the interval as argument. If NICE
is not NIL DATA and POINTS are revised to produce a nice set of values."
  (apply #'send interval-slider-dialog-proto :new args))

