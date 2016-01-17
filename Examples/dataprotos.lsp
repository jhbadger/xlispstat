;;;; A Data Set Prototype
(defproto data-set-proto '(data title))

(defmeth data-set-proto :isnew (data &key title)
  (send self :data data)
  (if title (send self :title title)))

(defmeth data-set-proto :title (&optional (title nil set))
"Method args: (&optional title)
Sets or retrieves the object's title."
  (if set (setf (slot-value 'title) title))
  (slot-value 'title))

(defmeth data-set-proto :data (&optional (data nil set))
"Method args: (&optional data)
Sets or retrieves the object's data."
  (if set (setf (slot-value 'data) data))
  (slot-value 'data))

(defmeth data-set-proto :describe (&optional (stream t))
"Method args: (&optional (stream t))
Prints a simple description of the object to STREAM."
  (let ((title (send self :title))
	       (data (send self :data)))
    (format stream "This is ~a~%" title)
    (format stream "The sample mean is ~g~%" (mean data))
    (format stream "The sample standard deviation is ~g~%"
            (standard-deviation data))))

(defmeth data-set-proto :plot () (histogram (send self :data)))

(send data-set-proto :title "a data set")

(defun make-data-set (x &key (title "a data set") (print t))
  (let ((object (send data-set-proto :new x :title title)))
    (if print (send object :describe))
    object))

;;;; A Time Series Prototype
(defproto time-series-proto () () data-set-proto)

(defmeth time-series-proto :plot ()
  (let ((data (send self :data)))
    (plot-points (iseq 0 (- (length data) 1)) data)))

(defmeth time-series-proto :describe (&optional (stream t))
  (call-next-method stream)
  (format stream 
          "The autocorrelation is ~g~%" 
          (autocorrelation (send self :data))))

(defun autocorrelation (x)
  (let* ((n (length x)))
    (/ (sum (* (select x (iseq 0 (- n 2))) 
               (select x (iseq 1 (- n 1)))))
       (sum (* x x)))))

(send time-series-proto :title "a time series")

(defun make-time-series (x &key (title "a data set") (print t))
  (let ((object (send time-series-proto :new x :title title)))
    (if print (send object :describe))
    object))

;;;; A Rectangular Data Set Prototype
(defproto rect-data-proto '(labels) () data-set-proto)

(defmeth rect-data-proto :isnew (data &key title labels)
  (let ((n (length data)))
    (send self :data data)
    (if title (send self :title title))
    (send self :labels
          (if labels
              labels
              (mapcar #'(lambda (x) (format nil "X~a" x)) 
                      (iseq 0 (- n 1)))))))

(defmeth rect-data-proto :labels (&optional (labels nil set))
  (if set (setf (slot-value 'labels) labels))
  (slot-value 'labels))

(send rect-data-proto :title "a rectangular data set")

;;;; A Data Set Instance
(setf x (send data-set-proto :new (chisq-rand 20 5)))

;;;; A Time Series Instance
(setf y (send time-series-proto :new
                (let ((e (normal-rand 21)))
                  (+ (select e (iseq 1 20)) 
                     (* .6 (select e (iseq 0 19)))))))

;;;; A Rectangular Data Set Instance
(setf z (send rect-data-proto :new (uniform-rand '(20 20 20 20))))
