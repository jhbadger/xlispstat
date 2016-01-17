; tests of arithmetic, logical, and bit funcitons
(setf eps 1.e-6)

;arith with integer args
(check #'= (+ 1 3) (* 2 2))
(check #'= (- (list -3 1789) 20) (list -23 1769))
(check #'= (* (list -7 09 -7 09) (list 100 -10 1 -9)) (- (list 700 90 7 81)))
(check #'< 
     (abs (- (/ (list 5 3 1 340) 3) (list 1.6666667 1. .3333333 113.3333333)))
     eps)
(check #'= (floor (/ (list 5 3 1 340) 3)) (list 1 1 0 113))
(check #'= (^ (list 12 35 159) 2) (list 144 1225 25281))
(check #'= (rem (list 86 -33 123456) 5) (list 1 -3 1))

;arith with real or mixed args
(check #'= (+ 15 .0078) 15.0078)
(check #'< (abs (- (- (list 23.4 1 -50) 17) (list 6.4 -16 -67))) eps)
(check #'< (abs (- (* 1.234e12 .02) 2.468e10)) eps)
(check #'< (abs (- (/ (list 15 -2 1.e3) 7.2) 
		(list 2.0833333 -.2777778 138.8888889))) 
	eps)
(check #'< (abs (- (rem 17.53 (list 5. 1.5)) (list 2.53 1.03))) eps)
(check #'< (abs (- (^ (list 1.2 5.67) 2.001) (list 1.440263 32.204733))) eps)

;arith with double complex args
(check #'= (+ 15 .0078e0) 15.0078)
(check #'< 
     (abs (- (- (list 23.4 1 -50) #c(17 5))
	     (list #c(6.4 -5) #c(-16 -5) #c(-67 -5))))
     eps)
(check #'< (abs (- (* 1.23456789012345e10 .02) 246913578.024690)) eps)
(check #'<
     (abs (- (/ (list 15 -2. 1.e3) 7.2) 
	     (list 2.0833333 -.2777778 138.8888889))) 
     eps)

;logical with integer args
(check #'eq (< 5 (list 4 6 -5)) (list nil T nil))
(check #'eq (> (list 7 12345) 500) (list nil T))
(check #'eq (<= 17 (list 17 -1 100)) (list T nil T))
(check #'eq (>= -12 (list 500 0 -5 -12 -30)) (list nil nil nil T T))
(check #'eq (= (list 2 3 -3 4) (list 2 -3 -3 1)) (list T nil T nil))

;logical with real or mixed arguments
(check #'eq (< 5 (list 4.9 6.123 -5)) (list nil T nil))
(check #'eq (> (list 7.3 12345) 7.3) (list nil T))
(check #'eq (> (list 7.3 12345) 7.3) (list nil T))
(check #'eq (<= 1.17 (list 1.17 -1.1 100.1)) (list T nil T))
(check #'eq 
     (>= -12.001 (list 500.001 0.001 -5.001 -12.001 -30))
     (list nil nil nil T T))
(check #'eq (not T) nil)
(check #'eq (not (and T nil)) T)
(check #'= (if-else (> (iseq 1 3) 2) 100 0) (list 0 0 100))
