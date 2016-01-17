; test data manipulation and subsetting
(setf eps 1.e-7
      xx (list 3 4.5 2 9 -6)
      yy (list 3 4 2 5 1)
      ll (combine (iseq 3 6) "jello" (list #c(1 1) #c(2 3))))

(check #'= 
       (mapcar #'mean (list (list 1 3 4 2) (list 7 6 8 9))) (list 2.5 7.5))

(check #'< (abs (- (if-else (= yy 2) yy 0) (list 0 0 2 0 0))) eps)

(let ((ord (order xx)))
  (check #'< (abs (- (sort-data xx) (select xx ord))) eps)
  (check #'< (abs (- (select yy ord) (iseq 1 5))) eps))

(check #'< (abs (- (select (repeat xx 3) 11) 4.5)) eps)
(check #'< (abs (- (repeat xx 3) (combine xx xx xx))) eps)
(check #'< (abs (- (select (repeat xx yy) (iseq 0 7)) 
		   (list 3 3 3 4.5 4.5 4.5 4.5 2))) eps)
(check #'< (abs (- (select (reverse (repeat xx yy)) (iseq 0 7)) 
		   (list -6 9 9 9 9 9 2 2))) eps)

#|
all(split(c("a" "b" "c" "d") c(1 2 1 2))[[2]]==c("b" "d"))
all(split(c("a" "b" "c" "d") c(1 2 1 2))$"2"==c("b" "d"))
|#
(check #'= (outer-product (iseq 1 3) (iseq 1 4))
       (matrix '(3 4) (combine (iseq 1 4) (* 2 (iseq 1 4)) (* 3 (iseq 1 4)))))
