(defun fib (x)
       (if (< x 2)
           x
           (+ (fib (1- x)) (fib (- x 2)))))

(defun fibi (n)
	(do	((i 1 (1+ i))
		 (fib-i-1 0 fib-i)
		 (fib-i   1 (+ fib-i fib-i-1)))
		((= i n) fib-i)))
