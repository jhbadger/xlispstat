(defun ack (m n)
	(cond ((zerop m) (1+ n))
	      ((zerop n) (ack (1- m) 1))
	      ((ack (1- m) (ack m (1- n))))))
