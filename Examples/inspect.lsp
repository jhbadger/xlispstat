(provide "inspect")

;;;
;;;
;;; Inspect Dialog Prototype
;;;
;;;

(defproto inspect-dialog-proto '(data editable) () dialog-proto)

(defmeth inspect-dialog-proto :isnew (d &key (title "Inspect") edit)
  (setf (slot-value 'data) d)
  (setf (slot-value 'editable) edit)
  (let ((items (append (send self :make-items)
                       (if edit 
                           (list (send button-item-proto :new "Edit" 
                                       :action
                                       #'(lambda () 
                                           (send self :edit-selection))))))))
    (call-next-method items :title title :type 'modeless :go-away t)))

(defmeth inspect-dialog-proto :make-items ()
  (let ((data (slot-value 'data)))
    (list (send text-item-proto :new (format nil "type:  ~s" (type-of data)))
          (send text-item-proto :new (format nil "value: ~s" data)))))

(defmeth inspect-dialog-proto :edit-selection () (sysbeep))

;;;
;;;
;;; Inspect Symbol Dialog Proto
;;;
;;;

(defproto inspect-symbol-dialog-proto '(list-item) () inspect-dialog-proto)

(defmeth inspect-symbol-dialog-proto :isnew (d &key (title "Inspect") edit)
  (if (not (symbolp d)) (error "not a symbol"))
  (call-next-method d :title title :editable edit))

(defmeth inspect-symbol-dialog-proto :make-items ()
  (let* ((data (slot-value 'data))
         (strings (list (format nil "name:      ~s" (symbol-name data))
                        (format nil "value:      ~s" 
                               (if (boundp data) 
                                   (symbol-value data) '*unbound*))
                        (format nil "function: ~s" 
                                (if (fboundp data)
                                    (symbol-function data) '*unbound*))
                        (format nil "plist:        ~s" (symbol-plist data)))))
    (setf (slot-value 'list-item)
          (send list-item-proto :new strings 
                :action	(let ((d self))
                          #'(lambda (double) 
                               (if double (send d :inspect-selection))))))
    (list (send text-item-proto :new (format nil "type:  ~s" (type-of data)))
          (slot-value 'list-item))))

(defmeth inspect-symbol-dialog-proto :inspect-selection ()
  (let ((data (slot-value 'data))
        (editable (slot-value 'editable)))
    (case (send (slot-value 'list-item) :selection)
          (0 (inspect (symbol-name data)))
          (1 (if (boundp data) 
                 (inspect (symbol-value data) :editable editable)))
          (2 (if (fboundp data) 
                 (inspect (symbol-function data) :editable editable)))
          (3 (if (symbol-plist data) 
                 (inspect (symbol-plist data) :editable editable))))))

(defmeth inspect-symbol-dialog-proto :edit-selection ()
  (let ((data (slot-value 'data)))
    (case (send list-item :selection)
          (1 (let ((v (get-value-dialog "New symbol-value")))
               (when v 
                     (setf (symbol-value data) (car v))
                     (send list-item :set-text 1 
                           (format nil "value:      ~s"  
                                   (symbol-value data))))))
          (2 (let ((v (get-value-dialog "New symbol-function")))
               (when v 
                     (setf (symbol-function data) (car v))
                     (send list-item :set-text 2 
                           (format nil "function: ~s" 
                                   (symbol-function data))))))
          (3 (let ((v (get-value-dialog "New symbol-plist")))
               (when v
                     (setf (symbol-plist data) (car v))
                     (send list-item :set-text 3 
                           (format nil "plist:        ~s"
                                   (symbol-plist data)))))))))

;;;
;;;
;;; Inspect Sequence Dialog proto
;;;
;;;

(defproto inspect-sequence-dialog-proto '(list-item) () inspect-dialog-proto)

(defmeth inspect-sequence-dialog-proto :isnew 
  (d &key (title "Inspect") edit)
  (if (not (or (consp d) (vectorp d))) (error "not a sequence"))
  (call-next-method d :title title :editable edit))

(defmeth inspect-sequence-dialog-proto :make-items ()
  (let* ((data (slot-value 'data))
         (strings (map-elements #'(lambda (x) (format nil "~s" x)) data)))
    (setf (slot-value 'list-item)
          (send list-item-proto :new strings 
                :action	(let ((d self))
                          #'(lambda (double) 
                                    (if double 
                                        (send d :inspect-selection))))))
    (list (send text-item-proto :new 
                (format nil "type:    ~s" (type-of data)))
          (send text-item-proto :new
                (format nil "length:  ~s" (length data)))
          (slot-value 'list-item))))

(defmeth inspect-sequence-dialog-proto :inspect-selection ()
  (let ((data (slot-value 'data))
        (editable (slot-value 'editable))
        (list-item (slot-value 'list-item)))
    (inspect (elt data (send list-item :selection)) :editable editable)))

(defmeth inspect-sequence-dialog-proto :edit-selection ()
  (let* ((data (slot-value 'data))
         (i (send list-item :selection))
         (v (get-value-dialog "New value for element")))
    (when v 
          (setf (elt data i) (car v))
          (send list-item :set-text i (format nil "~s" (elt data i))))))

;;;
;;;
;;; Inspect Matrix Dialog Proto
;;;
;;;

(defproto inspect-matrix-dialog-proto 
  '(list-item columns) () inspect-dialog-proto)

(defmeth inspect-matrix-dialog-proto :isnew (d &key (title "Inspect") edit)
  (if (not (matrixp d)) (error "not a matrix"))
  (setf (slot-value 'columns) (min 3 (array-dimension d 1)))
  (call-next-method d :title title :editable edit))

(defmeth inspect-matrix-dialog-proto :make-items ()
  (let* ((data (slot-value 'data))
         (columns (slot-value 'columns))
         (strings (map-elements #'(lambda (x) (format nil "~s" x)) data)))
    (setf (slot-value 'list-item) 
          (send list-item-proto :new strings :columns columns
                :action	#'(lambda (double) 
                            (if double (send self :inspect-selection)))))
    (list (send text-item-proto :new
                (format nil "type:    ~s" (type-of data)))
          (send text-item-proto :new
                (format nil "dimensions:  ~s" (array-dimensions data)))
          (slot-value 'list-item))))

(defmeth inspect-matrix-dialog-proto :inspect-selection ()
  (let ((data (slot-value 'data))
        (columns (slot-value 'columns)))
    (inspect (apply #'aref data (send (slot-value 'list-item) :selection)) 
             :editable (slot-value 'editable))))

(defmeth inspect-matrix-dialog-proto :edit-selection ()
  (let* ((data (slot-value 'data))
         (i (send list-item :selection))
         (v (get-value-dialog "New value for element")))
    (when v 
          (setf (aref data (car i) (cadr i)) (car v))
          (send list-item :set-text i 
                (format nil "~s" (aref data (car i) (cadr i)))))))

;;;
;;;
;;; Inspect Function
;;;
;;;

(defun inspect (x &rest args)
  (cond ((symbolp x) (apply #'send inspect-symbol-dialog-proto :new x args))
        ((or (consp x) (vectorp x))
         (apply #'send inspect-sequence-dialog-proto :new x args))
        ((matrixp x) (apply #'send inspect-matrix-dialog-proto :new x args))
        (t (apply #'send inspect-dialog-proto :new x args))))	
