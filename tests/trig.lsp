; test trigonometric functions
(setf eps 1.e-6	deg2rad (/ pi 180) rad2deg (/ 180 pi))

(check #'< (abs (- (* 2 pi rad2deg) 360)) eps)
(check #'< (abs (- (* (atan (/ 1 1)) rad2deg) 45)) eps)
(check #'< (abs (- (atan (/ 5 5)) (/ pi 4))) eps)
(check #'<
       (abs (- (sqrt (+ (^ 2 2) (^ 3 2)))  (/ 3 (sin (atan (/ 3 2)))))) eps)
(check #'<
       (abs (- (cos (* pi .37)) (/ (sin (* pi .37)) (tan (* pi .37))))) eps)
(check #'< (abs (- (cos (/ pi 4)) (sin (/ pi 4)))) eps)
(check #'< (abs (- (acos -1) pi)) eps)
(check #'< (abs (- (asin 1) (/ pi 2))) eps)

; fft test
(let* ((n 1000)
       (x (normal-rand n)))
  (check #'< (max (abs (- x (/ (fft (fft x) t) n)))) eps))
