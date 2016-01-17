(defun fact (n)
       (cond ((zerop n) 1)
	     ((= n 1) 1)
	     (t (* n (fact (- n 1))))))
(defun facti (n &aux (v 1)) ;; Iterative version
       (dotimes (i n) (setq v (* v (1+ i))))
       v)
