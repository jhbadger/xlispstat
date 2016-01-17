; test of matrix related functions (not very thorough)

(setf eps 1.e-7
      xmat (transpose (matrix '(4 6) (iseq 1 24)))
      xmat2 (permute-array xmat '(1 0)))

(check #'= (array-dimension (matrix '(4 1) (iseq 1 4)) 0) 4)
(check #'= (aref xmat2 0 1) (aref xmat 1 0))
(check #'= (mapcar #'mean (column-list xmat)) (mapcar #'mean (row-list xmat2)))

(check #'= 
       (bind-columns (iseq 1 3) (iseq 4 6)) (bind-rows '(1 4) '(2 5) '(3 6)))
(check #'= 
       (mapcar #'(lambda (x y) (select xmat x y)) '(0 1 2 3) '(0 1 2 3)) 
       (diagonal xmat))
(check #'= xmat (transpose xmat2))


#|
ymat matrix(seq(0.1,18),nrow=6)	# ray
cancor(xmat,ymat)$cor!=1			# ray begins
sum(hat(longley.x,F))==dim(longley.x)[2]
all(l1fit(matrix(rnorm(24),nrow=6),(iseq 1 6))$coef!=0)
all(leaps(matrix(rnorm(24),nrow=6),(iseq 1 6))$Cp!=0)
sum(ls.summary(ls.out_lsfit(matrix(runif(18),nrow=6),(iseq 1 6),int=F))$hat)-3 < eps
all(qr.coef(ls.out$qr,(iseq 1 6))!=0)
all(qr.fitted(ls.out$qr,(iseq 1 6))+qr.resid(ls.out$qr,(iseq 1 6))+qr.qty(ls.out$qr,(iseq 1 6))+qr.qy(ls.out$qr,(iseq 1 6))!=0)
all(apply(scale(ymat),2,"var")-1<eps)
mean(scale(ymat))<eps				# ray ends
{
	ymat <- matrix(c(1.01,2.02,-3.2,4.7,60,-14.3),ncol=2)
	ymat2 <- matrix(c(3.3,5.01,2.3,-7.4,-38,19.9),ncol=2)
	T
}
all(t(ymat)%*%ymat2-ymat%c%ymat2<eps)
all(crossprod(ymat,ymat2)-ymat%c%ymat2==0)
all(ymat+ymat2==ymat2+ymat)
|#

(flet ((tchol (x)
         (let ((c (chol-decomp x)))
	   (abs (- (matmult (first c) (transpose (first c))) x))))
       (tsvd (x) 
         (let ((s (sv-decomp x)))
	   (abs (- (matmult (first s)
			    (diagonal (second s))
			    (transpose (third s)))
		   x))))
       (teigen (x)
         (let ((e (eigen x)))
	   (abs (- (matmult (apply #'bind-columns (second e))
			    (diagonal (first e))
			    (transpose (apply #'bind-columns (second e))))
		   x)))))
#|
	tvar_function(x){(length(x)-1)*var(x)+length(x)*mean(x)^2-sum(x^2)}
	tvar2_function(x,y=x){n_nrow(x);(n-1)*var(x,y)-(x%c%y-n*outer(apply(x,2,mean),apply(y,2,mean)))}
	tcor_function(x,y=x){cor(x,y)-var(x,y)/sqrt(outer(diag(var(x)),diag(var(y))))}
|#
  (let* ((x (matrix '(50 10) (uniform-rand 500)))
	 (y (cross-product x))
	 (z1 (matrix '(10 3) (normal-rand 30)))
	 (z2 (matrix '(10 3) (uniform-rand 30))))
    (check #'< (tchol y) eps)
    (check #'< (tsvd x) eps)
    (check #'< (teigen y) eps)
    (check #'< (- (matmult (inverse y) y) (identity-matrix 10)) eps)))
#|
tvar(runif(100))<100*eps
all(tvar2(z1)<100*eps)
all(tvar2(z2)<100*eps)
all(tvar2(z1,z2)<100*eps)
all(tcor(x)<100*eps)
all(tcor(z1)<100*eps)
all(tcor(z2)<100*eps)
all(tcor(z1,z2)<100*eps)

{
	m_matrix((iseq 1 12),3,4)
	s_sweep(m,2,apply(m,2,mean))
	s2_sweep(m,1,apply(m,1,prod),'*')
	s3_matrix(8*c(35,220,729,140,550,1458,245,880,2187,350,1210,2916),3,4)
	T
}
all(dim(s)==dim(m))
all(s==rep(-(iseq 1 1),4))
all(dim(s2)==dim(s3))
all(s2==s3)
{
	s1_sample((iseq 1 10))
	s2_sample((iseq 1 10),20,replace=T)
	s3_sample((iseq 1 10000),10)
	T
}
all(sort(s1)==(iseq 1 10))
all(min(s2)>=1&&max(s2)<=10&&length(s2)==20)
all(min(s3)>=1&&max(s3)<=10000&&length(s3)==10)
|#

