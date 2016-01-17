(setf (aref *readtable* (char-int #\[))
      (cons :tmacro
	    (lambda (f c &aux ex)
		    (do () 
			((eq (peek-char t f) #\]))
			(setf ex (append ex (list (read f)))))
		    (read-char f)
		    (cons (cons 'send ex) nil))))

(setf (aref *readtable* (char-int #\]))
      (cons :tmacro
	    (lambda (f c)
		    (error "misplaced right bracket"))))


