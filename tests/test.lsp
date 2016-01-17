(setf *test-files*
      (list "arith" "complex" "manip" "matrix" "matrix2" "blas"
	    "math" "prob" "trig" "rans"))

(setf *testdir*
      (make-pathname :directory (pathname-directory *load-pathname*)
		     :device (pathname-device *load-pathname*)
		     :host (pathname-device *load-pathname*)))
      

(defun check (f a b)
  (flet ((as-list (x) 
           (if (compound-data-p x) (coerce (element-seq x) 'list) (list x))))
    (let ((res (as-list (map-elements f a b))))
      (if (member nil res)
	  (format t "test failed in ~s at ckeck ~d~%"
		  *current-test-file* count)))
    (setf count (+ count 1))))

(dolist (f *test-files*)
  (progv '(*current-test-file* count) (list f 1)
	 (load (merge-pathnames f *testdir*))))

	    