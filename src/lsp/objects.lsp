;;;;
;;;; objects.lsp XLISP-STAT additional objects and object functions
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;

(in-package "XLISP")
(provide "objects")

;;**** check over exports
(export '(edit-text-item-proto modal-dialog-proto modal-button-proto dash-item-proto))

(defsetf slot-value slot-value)

(defmeth *object* :new (&rest args)
"Method args: (&rest args)
Creates new object using self as prototype."
  (let* ((object (make-object self)))
    (if (slot-value 'instance-slots)
        (dolist (s (slot-value 'instance-slots))
                (send object :add-slot s (slot-value s))))
    (apply #'send object :isnew args)
    object))

(defmeth *object* :retype (proto &rest args)
"Method args: (proto &rest args)
Changes object to inherit directly from prototype PROTO. PROTO
must be a prototype and SELF must not be one."
  (if (send self :has-slot 'instance-slots :own t) 
      (error "can't retype a prototype"))
  (if (not (send proto :has-slot 'instance-slots :own t))
      (error "not a prototype - ~a" proto))
  (send self :reparent proto)
  (dolist (s (send proto :slot-value 'instance-slots))
    (send self :add-slot s (slot-value s)))
  (apply #'send self :isnew args)
  self)

(defmeth *object* :print (&optional (stream *standard-output*))
"Method args: (&optional (stream *standard-output*))
Default object printing method."
  (when *print-readably*
	(if (send self :has-method :save)
	    (format stream "#.~s" (send self :save))
	    (error "default :PRINT output is not readable")))
  (cond
    ((send self :has-slot 'proto-name) 
     (format stream
             "#<Object: ~D, prototype = ~A>"
             (address-of self)
             (slot-value 'proto-name)))
    (t (format stream "#<Object: ~D>" (address-of self)))))

(defmeth *object* :slot-value (sym &optional (val nil set))
"Method args: (sym &optional val)
Sets and retrieves value of slot named SYM. Sugnals an error if slot
does not exist."
  (if set (setf (slot-value sym) val))
  (slot-value sym))

(defmeth *object* :slot-names () 
"Method args: ()
Returns list of slots available to the object."
  (apply #'append 
         (mapcar #'(lambda (x) (send x :own-slots))
                 (send self :precedence-list))))

(defmeth *object* :method-selectors ()
"Method args: ()
Returns list of method selectors available to object."
  (apply #'append
         (mapcar #'(lambda (x) (send x :own-methods))
                 (send self :precedence-list))))

;;;;
;;;; More Hardware Object Methods
;;;;
(defmeth hardware-object-proto :remove () (send self :dispose))
(defmeth hardware-object-proto :allocated-p () (slot-value 'hardware-address))

(defmeth hardware-object-proto :add-subordinate (d)
  (setf (slot-value 'subordinates) (adjoin d (slot-value 'subordinates))))

(defmeth hardware-object-proto :delete-subordinate (d)
  (setf (slot-value 'subordinates) (remove d (slot-value 'subordinates))))
  
(defmeth hardware-object-proto :clobber ()
  (if (slot-value 'subordinates)
      (dolist (i (slot-value 'subordinates)) (send i :remove))))

#+macintosh
(progn
  (export 'display-window-proto)
  
  ;; DISPLAY-WINDOW-PROTO
  (defproto display-window-proto '() '() edit-window-proto)

  (defmeth display-window-proto :isnew (&rest args)
    (apply #'call-next-method args)
    (setf (slot-value 'input-enabled) nil)))

(export 'active-windows)

(defun active-windows ()
"Args: ()
Returns list of active windows."
    (remove-if-not #'(lambda (x) (kind-of-p x window-proto))
                   (mapcar #'third *hardware-objects*)))

;;;;
;;;; More Dialogs and Menu Items
;;;;

(send dialog-proto :slot-value 'type 'modeless)
(send dialog-proto :slot-value 'go-away t)

(defmeth dialog-proto :items () (slot-value 'items))

(defmeth dialog-item-proto :dialog () (slot-value 'dialog))

(defproto edit-text-item-proto () () text-item-proto)
(send edit-text-item-proto :slot-value 'editable t)

;;; MODAL-DIALOG-PROTO
(defproto modal-dialog-proto '(modal-throw-target) () dialog-proto)
(send modal-dialog-proto :slot-value 'type 'modal)
(send modal-dialog-proto :slot-value 'go-away nil)

(defmeth modal-dialog-proto :modal-dialog (&optional (remove t))
"Metod args: (&optional (remove t))
Runs the modal dialog loop until the :modal-dialog-return message
is sent. Returns the argument to :modal-dialog-return. If REMOVE
is not NIL, dialog is sent the :remove message before returning."
  (let ((target self))
    (unless (slot-value 'modal-throw-target)
            (setf (slot-value 'modal-throw-target) target)
            (send self :show-window)
            (unwind-protect (catch target 
                                   (loop (send (call-next-method) :do-action)))
                            (setf (slot-value 'modal-throw-target) nil)
                            (if remove (send self :remove))))))

(defmeth modal-dialog-proto :modal-dialog-return (value)
"Method Args: (value)
Ends modal dialog loop and has :modal-dialog return VALUE."
  (let ((target (slot-value 'modal-throw-target)))
    (if target (throw target value))))

;;; MODAL-BUTTON-PROTO
(defproto modal-button-proto '() () button-item-proto)
  
(defmeth modal-button-proto :do-action ()
  (let ((action (slot-value 'action))
        (dialog (slot-value 'dialog)))
    (if dialog 
        (send dialog :modal-dialog-return (if action (funcall action))))))

;; DASH-ITEM-PROTO. Disabled line item for separation 
(defproto dash-item-proto () () menu-item-proto "Disabled separator line")

(defmeth dash-item-proto :isnew () (call-next-method "-" :enabled nil))

(defmeth menu-item-proto :menu () 
"Method args: ()
Returns menu if item is installed, NIL otherwise."
  (slot-value 'menu))

(defmeth menu-proto :print (&optional (stream t))
  (format stream "#<Object: ~d, prototype = ~a, title = ~s>"
          (address-of self)
          (slot-value 'proto-name)
          (slot-value 'title)))

(defmeth menu-item-proto :print (&optional stream)
  (format stream "#<Object: ~d, prototype = ~a, title = ~s>"
          (address-of self)
          (slot-value 'proto-name)
          (slot-value 'title)))

(defmeth graph-window-proto :erase-window ()
"Method args: ()
Erases the entire window canvas."
  (let ((w (send self :canvas-width))
        (h (send self :canvas-height)))
    (send self :erase-rect 0 0 w h)))
