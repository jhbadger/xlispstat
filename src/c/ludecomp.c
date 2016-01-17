/* LU Decomposition from LINPACK.  Translated by f2c and modified. */

#include "linalg.h"

VOID linpack_dgeco P6C(double *, a,
		       int, lda,
		       int, n,
		       int *, ipvt,
		       double *, rcond,
		       double *, z)
{
  /* System generated locals */
  int a_dim1, a_offset, i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  int info;
  int j, k, l;
  double s, t;
  double anorm;
  double ynorm;
  int kb;
  double ek, sm, wk;
  int kp1;
  double wkm;

  /* dgeco factors a double precision matrix by gaussian elimination */
  /* and estimates the condition of the matrix. */

  /* if  rcond  is not needed, dgefa is slightly faster. */
  /* to solve  a*x = b , follow dgeco by dgesl. */
  /* to compute  inverse(a)*c , follow dgeco by dgesl. */
  /* to compute  determinant(a) , follow dgeco by dgedi. */
  /* to compute  inverse(a) , follow dgeco by dgedi. */

  /* on entry */

  /*    a       double precision(lda, n) */
  /*            the matrix to be factored. */

  /*    lda     integer */
  /*            the leading dimension of the array  a . */

  /*    n       integer */
  /*            the order of the matrix  a . */

  /* on return */

  /*    a       an upper triangular matrix and the multipliers */
  /*            which were used to obtain it. */
  /*            the factorization can be written  a = l*u  where */
  /*            l  is a product of permutation and unit lower */
  /*            triangular matrices and  u  is upper triangular. */

  /*    ipvt    integer(n) */
  /*            an integer vector of pivot indices. */

  /*    rcond   double precision */
  /*            an estimate of the reciprocal condition of  a . */
  /*            for the system  a*x = b , relative perturbations */
  /*            in  a  and  b  of size  epsilon  may cause */
  /*            relative perturbations in  x  of size  epsilon/rcond. */
  /*            if  rcond  is so small that the logical expression */
  /*                       1.0 + rcond .eq. 1.0 */
  /*            is true, then  a  may be singular to working */
  /*            precision.  in particular,  rcond  is zero  if */
  /*            exact singularity is detected or the estimate */
  /*            underflows. */

  /*    z       double precision(n) */
  /*            a work vector whose contents are usually unimportant. */
  /*            if  a  is close to a singular matrix, then  z  is */
  /*            an approximate null vector in the sense that */
  /*            norm(a*z) = rcond*norm(a)*norm(z) . */

  /* linpack. this version dated 08/14/78 . */
  /* cleve moler, university of new mexico, argonne national lab. */

  /* subroutines and functions */

  /* linpack dgefa */
  /* blas daxpy,ddot,dscal,dasum */
  /* fortran dabs,dmax1,dsign */

  /* Parameter adjustments */
  a_dim1 = lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --ipvt;
  --z;

  /* compute 1-norm of a */

  anorm = 0.;
  i__1 = n;
  for (j = 1; j <= i__1; ++j) {
    /* Computing MAX */
    d__1 = anorm, d__2 = blas_dasum(n, &a[j * a_dim1 + 1], 1);
    anorm = max(d__1,d__2);
    /* L10: */
  }

  /* factor */

  linpack_dgefa(&a[a_offset], lda, n, &ipvt[1], &info);

  /* rcond = 1/(norm(a)*(estimate of norm(inverse(a)))) . */
  /* estimate = norm(z)/norm(y) where  a*z = y  and  trans(a)*y = e . */
  /* trans(a)  is the transpose of a .  the components of  e  are */
  /* chosen to cause maximum local growth in the elements of w  where */
  /* trans(u)*w = e .  the vectors are frequently rescaled to avoid */
  /* overflow. */

  /* solve trans(u)*w = e */

  ek = 1.;
  i__1 = n;
  for (j = 1; j <= i__1; ++j) {
    z[j] = 0.;
    /* L20: */
  }
  i__1 = n;
  for (k = 1; k <= i__1; ++k) {
    if (z[k] != 0.) {
      d__1 = -z[k];
      ek = d_sign(&ek, &d__1);
    }
    if ((d__1 = ek - z[k], abs(d__1)) <= (d__2 = a[k + k * a_dim1], abs(
									d__2))) {
      goto L30;
    }
    s = (d__1 = a[k + k * a_dim1], abs(d__1)) / (d__2 = ek - z[k], abs(
								       d__2));
    blas_dscal(n, s, &z[1], 1);
    ek = s * ek;
  L30:
    wk = ek - z[k];
    wkm = -ek - z[k];
    s = abs(wk);
    sm = abs(wkm);
    if (a[k + k * a_dim1] == 0.) {
      goto L40;
    }
    wk /= a[k + k * a_dim1];
    wkm /= a[k + k * a_dim1];
    goto L50;
  L40:
    wk = 1.;
    wkm = 1.;
  L50:
    kp1 = k + 1;
    if (kp1 > n) {
      goto L90;
    }
    i__2 = n;
    for (j = kp1; j <= i__2; ++j) {
      sm += (d__1 = z[j] + wkm * a[k + j * a_dim1], abs(d__1));
      z[j] += wk * a[k + j * a_dim1];
      s += (d__1 = z[j], abs(d__1));
      /* L60: */
    }
    if (s >= sm) {
      goto L80;
    }
    t = wkm - wk;
    wk = wkm;
    i__2 = n;
    for (j = kp1; j <= i__2; ++j) {
      z[j] += t * a[k + j * a_dim1];
      /* L70: */
    }
  L80:
  L90:
    z[k] = wk;
    /* L100: */
  }
  s = 1. / blas_dasum(n, &z[1], 1);
  blas_dscal(n, s, &z[1], 1);

  /* solve trans(l)*y = w */

  i__1 = n;
  for (kb = 1; kb <= i__1; ++kb) {
    k = n + 1 - kb;
    if (k < n) {
      i__2 = n - k;
      z[k] += blas_ddot(i__2, &a[k + 1 + k * a_dim1], 1, &z[k + 1], 1);
    }
    if ((d__1 = z[k], abs(d__1)) <= 1.) {
      goto L110;
    }
    s = 1. / (d__1 = z[k], abs(d__1));
    blas_dscal(n, s, &z[1], 1);
  L110:
    l = ipvt[k];
    t = z[l];
    z[l] = z[k];
    z[k] = t;
    /* L120: */
  }
  s = 1. / blas_dasum(n, &z[1], 1);
  blas_dscal(n, s, &z[1], 1);

  ynorm = 1.;

  /* solve l*v = y */

  i__1 = n;
  for (k = 1; k <= i__1; ++k) {
    l = ipvt[k];
    t = z[l];
    z[l] = z[k];
    z[k] = t;
    if (k < n) {
      i__2 = n - k;
      blas_daxpy(i__2, t, &a[k + 1 + k * a_dim1], 1, &z[k + 1], 1);
    }
    if ((d__1 = z[k], abs(d__1)) <= 1.) {
      goto L130;
    }
    s = 1. / (d__1 = z[k], abs(d__1));
    blas_dscal(n, s, &z[1], 1);
    ynorm = s * ynorm;
  L130:
    /* L140: */
    ;
  }
  s = 1. / blas_dasum(n, &z[1], 1);
  blas_dscal(n, s, &z[1], 1);
  ynorm = s * ynorm;

  /* solve  u*z = v */

  i__1 = n;
  for (kb = 1; kb <= i__1; ++kb) {
    k = n + 1 - kb;
    if ((d__1 = z[k], abs(d__1)) <= (d__2 = a[k + k * a_dim1], abs(d__2)))
      {
	goto L150;
      }
    s = (d__1 = a[k + k * a_dim1], abs(d__1)) / (d__2 = z[k], abs(d__2));
    blas_dscal(n, s, &z[1], 1);
    ynorm = s * ynorm;
  L150:
    if (a[k + k * a_dim1] != 0.) {
      z[k] /= a[k + k * a_dim1];
    }
    if (a[k + k * a_dim1] == 0.) {
      z[k] = 1.;
    }
    t = -z[k];
    i__2 = k - 1;
    blas_daxpy(i__2, t, &a[k * a_dim1 + 1], 1, &z[1], 1);
    /* L160: */
  }
  /* make znorm = 1.0 */
  s = 1. / blas_dasum(n, &z[1], 1);
  blas_dscal(n, s, &z[1], 1);
  ynorm = s * ynorm;

  if (anorm != 0.) {
    *rcond = ynorm / anorm;
  }
  if (anorm == 0.) {
    *rcond = 0.;
  }
  return;
}

VOID linpack_dgedi P7C(double *, a,
		       int, lda,
		       int, n,
		       int *, ipvt,
		       double *, det,
		       double *, work,
		       int, job)
{
  /* System generated locals */
  int a_dim1, a_offset, i__1, i__2;

  /* Local variables */
  int i, j, k, l;
  double t;
  int kb, kp1, nm1;
  double ten;

  /* dgedi computes the determinant and inverse of a matrix */
  /* using the factors computed by dgeco or dgefa. */

  /* on entry */

  /*    a       double precision(lda, n) */
  /*            the output from dgeco or dgefa. */

  /*    lda     integer */
  /*            the leading dimension of the array  a . */

  /*    n       integer */
  /*            the order of the matrix  a . */

  /*    ipvt    integer(n) */
  /*            the pivot vector from dgeco or dgefa. */

  /*    work    double precision(n) */
  /*            work vector.  contents destroyed. */

  /*    job     integer */
  /*            = 11   both determinant and inverse. */
  /*            = 01   inverse only. */
  /*            = 10   determinant only. */

  /* on return */

  /*    a       inverse of original matrix if requested. */
  /*            otherwise unchanged. */

  /*    det     double precision(2) */
  /*            determinant of original matrix if requested. */
  /*            otherwise not referenced. */
  /*            determinant = det(1) * 10.0**det(2) */
  /*            with  1.0 .le. dabs(det(1)) .lt. 10.0 */
  /*            or  det(1) .eq. 0.0 . */

  /* error condition */

  /*    a division by zero will occur if the input factor contains */
  /*    a zero on the diagonal and the inverse is requested. */
  /*    it will not occur if the subroutines are called correctly */
  /*    and if dgeco has set rcond .gt. 0.0 or dgefa has set */
  /*    info .eq. 0 . */

  /* linpack. this version dated 08/14/78 . */
  /* cleve moler, university of new mexico, argonne national lab. */

  /* subroutines and functions */

  /* blas daxpy,dscal,dswap */
  /* fortran dabs,mod */

  /* Parameter adjustments */
  a_dim1 = lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --ipvt;
  --det;
  --work;

  /* compute determinant */

  if (job / 10 == 0) {
    goto L70;
  }
  det[1] = 1.;
  det[2] = 0.;
  ten = 10.;
  i__1 = n;
  for (i = 1; i <= i__1; ++i) {
    if (ipvt[i] != i) {
      det[1] = -det[1];
    }
    det[1] = a[i + i * a_dim1] * det[1];
    /*    ...exit */
    if (det[1] == 0.) {
      goto L60;
    }
  L10:
    if (abs(det[1]) >= 1.) {
      goto L20;
    }
    det[1] = ten * det[1];
    det[2] += -1.;
    goto L10;
  L20:
  L30:
    if (abs(det[1]) < ten) {
      goto L40;
    }
    det[1] /= ten;
    det[2] += 1.;
    goto L30;
  L40:
    /* L50: */
    ;
  }
 L60:
 L70:

  /* compute inverse(u) */

  if (job % 10 == 0) {
    goto L150;
  }
  i__1 = n;
  for (k = 1; k <= i__1; ++k) {
    a[k + k * a_dim1] = 1. / a[k + k * a_dim1];
    t = -a[k + k * a_dim1];
    i__2 = k - 1;
    blas_dscal(i__2, t, &a[k * a_dim1 + 1], 1);
    kp1 = k + 1;
    if (n < kp1) {
      goto L90;
    }
    i__2 = n;
    for (j = kp1; j <= i__2; ++j) {
      t = a[k + j * a_dim1];
      a[k + j * a_dim1] = 0.;
      blas_daxpy(k, t, &a[k * a_dim1 + 1], 1, &a[j * a_dim1 + 1], 1);
      /* L80: */
    }
  L90:
    /* L100: */
    ;
  }

  /*    form inverse(u)*inverse(l) */

  nm1 = n - 1;
  if (nm1 < 1) {
    goto L140;
  }
  i__1 = nm1;
  for (kb = 1; kb <= i__1; ++kb) {
    k = n - kb;
    kp1 = k + 1;
    i__2 = n;
    for (i = kp1; i <= i__2; ++i) {
      work[i] = a[i + k * a_dim1];
      a[i + k * a_dim1] = 0.;
      /* L110: */
    }
    i__2 = n;
    for (j = kp1; j <= i__2; ++j) {
      t = work[j];
      blas_daxpy(n, t, &a[j * a_dim1 + 1], 1, &a[k * a_dim1 + 1], 1);
      /* L120: */
    }
    l = ipvt[k];
    if (l != k) {
      blas_dswap(n, &a[k * a_dim1 + 1], 1, &a[l * a_dim1 + 1], 1);
    }
    /* L130: */
  }
 L140:
 L150:
  return;
}

VOID linpack_dgefa P5C(double *, a,
		       int, lda,
		       int, n,
		       int *, ipvt,
		       int *, info)
{
  /* System generated locals */
  int a_dim1, a_offset, i__1, i__2, i__3;

  /* Local variables */
  int j, k, l;
  double t;
  int kp1, nm1;

  /* dgefa factors a double precision matrix by gaussian elimination. */

  /* dgefa is usually called by dgeco, but it can be called */
  /* directly with a saving in time if  rcond  is not needed. */
  /* (time for dgeco) = (1 + 9/n)*(time for dgefa) . */

  /* on entry */

  /*    a       double precision(lda, n) */
  /*            the matrix to be factored. */

  /*    lda     integer */
  /*            the leading dimension of the array  a . */

  /*    n       integer */
  /*            the order of the matrix  a . */

  /* on return */

  /*    a       an upper triangular matrix and the multipliers */
  /*            which were used to obtain it. */
  /*            the factorization can be written  a = l*u  where */
  /*            l  is a product of permutation and unit lower */
  /*            triangular matrices and  u  is upper triangular. */

  /*    ipvt    integer(n) */
  /*            an integer vector of pivot indices. */

  /*    info    integer */
  /*            = 0  normal value. */
  /*            = k  if  u(k,k) .eq. 0.0 .  this is not an error */
  /*                 condition for this subroutine, but it does */
  /*                 indicate that dgesl or dgedi will divide by zero */
  /*                 if called.  use  rcond  in dgeco for a reliable */
  /*                 indication of singularity. */

  /* linpack. this version dated 08/14/78 . */
  /* cleve moler, university of new mexico, argonne national lab. */

  /* subroutines and functions */

  /* blas daxpy,dscal,idamax */

  /* Parameter adjustments */
  a_dim1 = lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --ipvt;

  /* gaussian elimination with partial pivoting */

  *info = 0;
  nm1 = n - 1;
  if (nm1 < 1) {
    goto L70;
  }
  i__1 = nm1;
  for (k = 1; k <= i__1; ++k) {
    kp1 = k + 1;

    /*    find l = pivot index */

    i__2 = n - k + 1;
    l = blas_idamax(i__2, &a[k + k * a_dim1], 1) + k;
    ipvt[k] = l;

    /*    zero pivot implies this column already triangularized */

    if (a[l + k * a_dim1] == 0.) {
      goto L40;
    }

    /*       interchange if necessary */

    if (l == k) {
      goto L10;
    }
    t = a[l + k * a_dim1];
    a[l + k * a_dim1] = a[k + k * a_dim1];
    a[k + k * a_dim1] = t;
  L10:

    /*       compute multipliers */

    t = -1. / a[k + k * a_dim1];
    i__2 = n - k;
    blas_dscal(i__2, t, &a[k + 1 + k * a_dim1], 1);

    /*       row elimination with column indexing */

    i__2 = n;
    for (j = kp1; j <= i__2; ++j) {
      t = a[l + j * a_dim1];
      if (l == k) {
	goto L20;
      }
      a[l + j * a_dim1] = a[k + j * a_dim1];
      a[k + j * a_dim1] = t;
    L20:
      i__3 = n - k;
      blas_daxpy(i__3, t, &a[k + 1 + k * a_dim1], 1, &a[k + 1 + j * 
							a_dim1], 1);
      /* L30: */
    }
    goto L50;
  L40:
    *info = k;
  L50:
    /* L60: */
    ;
  }
 L70:
  ipvt[n] = n;
  if (a[n + n * a_dim1] == 0.) {
    *info = n;
  }
  return;
}

VOID linpack_dgesl P6C(double *, a,
		       int, lda,
		       int, n,
		       int *, ipvt,
		       double *, b,
		       int, job)
{
  /* System generated locals */
  int a_dim1, a_offset, i__1, i__2;

  /* Local variables */
  int k, l;
  double t;
  int kb, nm1;

  /* dgesl solves the double precision system */
  /* a * x = b  or  trans(a) * x = b */
  /* using the factors computed by dgeco or dgefa. */

  /* on entry */

  /*    a       double precision(lda, n) */
  /*            the output from dgeco or dgefa. */

  /*    lda     integer */
  /*            the leading dimension of the array  a . */

  /*    n       integer */
  /*            the order of the matrix  a . */

  /*    ipvt    integer(n) */
  /*            the pivot vector from dgeco or dgefa. */

  /*    b       double precision(n) */
  /*            the right hand side vector. */

  /*    job     integer */
  /*            = 0         to solve  a*x = b , */
  /*            = nonzero   to solve  trans(a)*x = b  where */
  /*                        trans(a)  is the transpose. */

  /* on return */

  /*    b       the solution vector  x . */

  /* error condition */

  /*    a division by zero will occur if the input factor contains a */
  /*    zero on the diagonal.  technically this indicates singularity */
  /*    but it is often caused by improper arguments or improper */
  /*    setting of lda .  it will not occur if the subroutines are */
  /*    called correctly and if dgeco has set rcond .gt. 0.0 */
  /*    or dgefa has set info .eq. 0 . */

  /* to compute  inverse(a) * c  where  c  is a matrix */
  /* with  p  columns */
  /*       call dgeco(a,lda,n,ipvt,rcond,z) */
  /*       if (rcond is too small) go to ... */
  /*       do 10 j = 1, p */
  /*          call dgesl(a,lda,n,ipvt,c(1,j),0) */
  /*    10 continue */

  /* linpack. this version dated 08/14/78 . */
  /* cleve moler, university of new mexico, argonne national lab. */

  /* subroutines and functions */

  /* blas daxpy,ddot */

  /* Parameter adjustments */
  a_dim1 = lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --ipvt;
  --b;

  /* Function Body */
  nm1 = n - 1;
  if (job != 0) {
    goto L50;
  }

  /*    job = 0 , solve  a * x = b */
  /*    first solve  l*y = b */

  if (nm1 < 1) {
    goto L30;
  }
  i__1 = nm1;
  for (k = 1; k <= i__1; ++k) {
    l = ipvt[k];
    t = b[l];
    if (l == k) {
      goto L10;
    }
    b[l] = b[k];
    b[k] = t;
  L10:
    i__2 = n - k;
    blas_daxpy(i__2, t, &a[k + 1 + k * a_dim1], 1, &b[k + 1], 1);
    /* L20: */
  }
 L30:

  /*    now solve  u*x = y */

  i__1 = n;
  for (kb = 1; kb <= i__1; ++kb) {
    k = n + 1 - kb;
    b[k] /= a[k + k * a_dim1];
    t = -b[k];
    i__2 = k - 1;
    blas_daxpy(i__2, t, &a[k * a_dim1 + 1], 1, &b[1], 1);
    /* L40: */
  }
  goto L100;
 L50:

  /*    job = nonzero, solve  trans(a) * x = b */
  /*    first solve  trans(u)*y = b */

  i__1 = n;
  for (k = 1; k <= i__1; ++k) {
    i__2 = k - 1;
    t = blas_ddot(i__2, &a[k * a_dim1 + 1], 1, &b[1], 1);
    b[k] = (b[k] - t) / a[k + k * a_dim1];
    /* L60: */
  }

  /*    now solve trans(l)*x = y */

  if (nm1 < 1) {
    goto L90;
  }
  i__1 = nm1;
  for (kb = 1; kb <= i__1; ++kb) {
    k = n - kb;
    i__2 = n - k;
    b[k] += blas_ddot(i__2, &a[k + 1 + k * a_dim1], 1, &b[k + 1], 1);
    l = ipvt[k];
    if (l == k) {
      goto L70;
    }
    t = b[l];
    b[l] = b[k];
    b[k] = t;
  L70:
    /* L80: */
    ;
  }
 L90:
 L100:
  return;
}

VOID linpack_zgeco P6C(dcomplex *, a,
		       int, lda,
		       int, n,
		       int *, ipvt,
		       double *, rcond,
		       dcomplex *, z)
{
  /* System generated locals */
  int a_dim1, a_offset, i__1, i__2, i__3, i__4, i__5;
  double d__1, d__2, d__3, d__4, d__5, d__6, d__7, d__8;
  dcomplex z__1, z__2, z__3, z__4, z__5, z__6, z__7;

  /* Local variables */
  int info, j, k, l;
  double s;
  dcomplex t;
  double anorm;
  double ynorm;
  int kb;
  dcomplex ek;
  double sm;
  dcomplex wk;
  int kp1;
  dcomplex wkm;

  /* zgeco factors a complex*16 matrix by gaussian elimination */
  /* and estimates the condition of the matrix. */

  /* if  rcond  is not needed, zgefa is slightly faster. */
  /* to solve  a*x = b , follow zgeco by zgesl. */
  /* to compute  inverse(a)*c , follow zgeco by zgesl. */
  /* to compute  determinant(a) , follow zgeco by zgedi. */
  /* to compute  inverse(a) , follow zgeco by zgedi. */

  /* on entry */

  /*    a       complex*16(lda, n) */
  /*            the matrix to be factored. */

  /*    lda     integer */
  /*            the leading dimension of the array  a . */

  /*    n       integer */
  /*            the order of the matrix  a . */

  /* on return */

  /*    a       an upper triangular matrix and the multipliers */
  /*            which were used to obtain it. */
  /*            the factorization can be written  a = l*u  where */
  /*            l  is a product of permutation and unit lower */
  /*            triangular matrices and  u  is upper triangular. */

  /*    ipvt    integer(n) */
  /*            an integer vector of pivot indices. */

  /*    rcond   double precision */
  /*            an estimate of the reciprocal condition of  a . */
  /*            for the system  a*x = b , relative perturbations */
  /*            in  a  and  b  of size  epsilon  may cause */
  /*            relative perturbations in  x  of size  epsilon/rcond. */
  /*            if  rcond  is so small that the logical expression */
  /*                       1.0 + rcond .eq. 1.0 */
  /*            is true, then  a  may be singular to working */
  /*            precision.  in particular,  rcond  is zero  if */
  /*            exact singularity is detected or the estimate */
  /*            underflows. */

  /*    z       complex*16(n) */
  /*            a work vector whose contents are usually unimportant. */
  /*            if  a  is close to a singular matrix, then  z  is */
  /*            an approximate null vector in the sense that */
  /*            norm(a*z) = rcond*norm(a)*norm(z) . */

  /* linpack. this version dated 08/14/78 . */
  /* cleve moler, university of new mexico, argonne national lab. */

  /* subroutines and functions */

  /* linpack zgefa */
  /* blas zaxpy,zdotc,zdscal,dzasum */
  /* fortran dabs,dmax1,dcmplx,dconjg */

  /* Parameter adjustments */
  a_dim1 = lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --ipvt;
  --z;

  /* compute 1-norm of a */

  anorm = 0.;
  i__1 = n;
  for (j = 1; j <= i__1; ++j) {
    /* Computing MAX */
    d__1 = anorm, d__2 = blas_dzasum(n, &a[j * a_dim1 + 1], 1);
    anorm = max(d__1,d__2);
    /* L10: */
  }

  /* factor */

  linpack_zgefa(&a[a_offset], lda, n, &ipvt[1], &info);

  /* rcond = 1/(norm(a)*(estimate of norm(inverse(a)))) . */
  /* estimate = norm(z)/norm(y) where  a*z = y  and  ctrans(a)*y = e. */
  /* ctrans(a)  is the conjugate transpose of a . */
  /* the components of  e  are chosen to cause maximum local */
  /* growth in the elements of w  where  ctrans(u)*w = e . */
  /* the vectors are frequently rescaled to avoid overflow. */

  /* solve ctrans(u)*w = e */

  ek.r = 1., ek.i = 0.;
  i__1 = n;
  for (j = 1; j <= i__1; ++j) {
    i__2 = j;
    z[i__2].r = 0., z[i__2].i = 0.;
    /* L20: */
  }
  i__1 = n;
  for (k = 1; k <= i__1; ++k) {
    i__2 = k;
    i__3 = k;
    z__1.r = z[i__3].r * 0. - z[i__3].i * -1., z__1.i = z[i__3].r * -1. + 
      z[i__3].i * 0.;
    if ((d__1 = z[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) != 0.) 
      {
	i__4 = k;
	z__3.r = -z[i__4].r, z__3.i = -z[i__4].i;
	z__2.r = z__3.r, z__2.i = z__3.i;
	z__5.r = ek.r * 0. - ek.i * -1., z__5.i = ek.r * -1. + ek.i * 0.;
	d__7 = (d__3 = ek.r, abs(d__3)) + (d__4 = z__5.r, abs(d__4));
	z__7.r = z__2.r * 0. - z__2.i * -1., z__7.i = z__2.r * -1. + 
	  z__2.i * 0.;
	d__8 = (d__5 = z__2.r, abs(d__5)) + (d__6 = z__7.r, abs(d__6));
	z__6.r = z__2.r / d__8, z__6.i = z__2.i / d__8;
	z__4.r = d__7 * z__6.r, z__4.i = d__7 * z__6.i;
	ek.r = z__4.r, ek.i = z__4.i;
      }
    i__2 = k;
    z__2.r = ek.r - z[i__2].r, z__2.i = ek.i - z[i__2].i;
    z__1.r = z__2.r, z__1.i = z__2.i;
    z__3.r = z__1.r * 0. - z__1.i * -1., z__3.i = z__1.r * -1. + z__1.i * 
      0.;
    i__3 = k + k * a_dim1;
    i__4 = k + k * a_dim1;
    z__4.r = a[i__4].r * 0. - a[i__4].i * -1., z__4.i = a[i__4].r * -1. + 
      a[i__4].i * 0.;
    if ((d__1 = z__1.r, abs(d__1)) + (d__2 = z__3.r, abs(d__2)) <= (d__3 =
								    a[i__3].r, abs(d__3)) + (d__4 = z__4.r, abs(d__4))) {
      goto L30;
    }
    i__2 = k;
    z__2.r = ek.r - z[i__2].r, z__2.i = ek.i - z[i__2].i;
    z__1.r = z__2.r, z__1.i = z__2.i;
    i__3 = k + k * a_dim1;
    i__4 = k + k * a_dim1;
    z__3.r = a[i__4].r * 0. - a[i__4].i * -1., z__3.i = a[i__4].r * -1. + 
      a[i__4].i * 0.;
    z__4.r = z__1.r * 0. - z__1.i * -1., z__4.i = z__1.r * -1. + z__1.i * 
      0.;
    s = ((d__1 = a[i__3].r, abs(d__1)) + (d__2 = z__3.r, abs(d__2))) / ((
									 d__3 = z__1.r, abs(d__3)) + (d__4 = z__4.r, abs(d__4)));
    blas_zdscal(n, s, &z[1], 1);
    z__2.r = s, z__2.i = 0.;
    z__1.r = z__2.r * ek.r - z__2.i * ek.i, z__1.i = z__2.r * ek.i + 
      z__2.i * ek.r;
    ek.r = z__1.r, ek.i = z__1.i;
  L30:
    i__2 = k;
    z__1.r = ek.r - z[i__2].r, z__1.i = ek.i - z[i__2].i;
    wk.r = z__1.r, wk.i = z__1.i;
    z__2.r = -ek.r, z__2.i = -ek.i;
    i__2 = k;
    z__1.r = z__2.r - z[i__2].r, z__1.i = z__2.i - z[i__2].i;
    wkm.r = z__1.r, wkm.i = z__1.i;
    z__1.r = wk.r * 0. - wk.i * -1., z__1.i = wk.r * -1. + wk.i * 0.;
    s = (d__1 = wk.r, abs(d__1)) + (d__2 = z__1.r, abs(d__2));
    z__1.r = wkm.r * 0. - wkm.i * -1., z__1.i = wkm.r * -1. + wkm.i * 0.;
    sm = (d__1 = wkm.r, abs(d__1)) + (d__2 = z__1.r, abs(d__2));
    i__2 = k + k * a_dim1;
    i__3 = k + k * a_dim1;
    z__1.r = a[i__3].r * 0. - a[i__3].i * -1., z__1.i = a[i__3].r * -1. + 
      a[i__3].i * 0.;
    if ((d__1 = a[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) 
      {
	goto L40;
      }
    d_cnjg(&z__2, &a[k + k * a_dim1]);
    z_div(&z__1, &wk, &z__2);
    wk.r = z__1.r, wk.i = z__1.i;
    d_cnjg(&z__2, &a[k + k * a_dim1]);
    z_div(&z__1, &wkm, &z__2);
    wkm.r = z__1.r, wkm.i = z__1.i;
    goto L50;
  L40:
    wk.r = 1., wk.i = 0.;
    wkm.r = 1., wkm.i = 0.;
  L50:
    kp1 = k + 1;
    if (kp1 > n) {
      goto L90;
    }
    i__2 = n;
    for (j = kp1; j <= i__2; ++j) {
      i__3 = j;
      d_cnjg(&z__4, &a[k + j * a_dim1]);
      z__3.r = wkm.r * z__4.r - wkm.i * z__4.i, z__3.i = wkm.r * z__4.i 
	+ wkm.i * z__4.r;
      z__2.r = z[i__3].r + z__3.r, z__2.i = z[i__3].i + z__3.i;
      z__1.r = z__2.r, z__1.i = z__2.i;
      z__5.r = z__1.r * 0. - z__1.i * -1., z__5.i = z__1.r * -1. + 
	z__1.i * 0.;
      sm += (d__1 = z__1.r, abs(d__1)) + (d__2 = z__5.r, abs(d__2));
      i__3 = j;
      i__4 = j;
      d_cnjg(&z__3, &a[k + j * a_dim1]);
      z__2.r = wk.r * z__3.r - wk.i * z__3.i, z__2.i = wk.r * z__3.i + 
	wk.i * z__3.r;
      z__1.r = z[i__4].r + z__2.r, z__1.i = z[i__4].i + z__2.i;
      z[i__3].r = z__1.r, z[i__3].i = z__1.i;
      i__3 = j;
      i__4 = j;
      z__1.r = z[i__4].r * 0. - z[i__4].i * -1., z__1.i = z[i__4].r * 
	-1. + z[i__4].i * 0.;
      s += (d__1 = z[i__3].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2));
      /* L60: */
    }
    if (s >= sm) {
      goto L80;
    }
    z__1.r = wkm.r - wk.r, z__1.i = wkm.i - wk.i;
    t.r = z__1.r, t.i = z__1.i;
    wk.r = wkm.r, wk.i = wkm.i;
    i__2 = n;
    for (j = kp1; j <= i__2; ++j) {
      i__3 = j;
      i__4 = j;
      d_cnjg(&z__3, &a[k + j * a_dim1]);
      z__2.r = t.r * z__3.r - t.i * z__3.i, z__2.i = t.r * z__3.i + t.i 
	* z__3.r;
      z__1.r = z[i__4].r + z__2.r, z__1.i = z[i__4].i + z__2.i;
      z[i__3].r = z__1.r, z[i__3].i = z__1.i;
      /* L70: */
    }
  L80:
  L90:
    i__2 = k;
    z[i__2].r = wk.r, z[i__2].i = wk.i;
    /* L100: */
  }
  s = 1. / blas_dzasum(n, &z[1], 1);
  blas_zdscal(n, s, &z[1], 1);

  /* solve ctrans(l)*y = w */

  i__1 = n;
  for (kb = 1; kb <= i__1; ++kb) {
    k = n + 1 - kb;
    if (k < n) {
      i__2 = k;
      i__3 = k;
      i__4 = n - k;
      blas_zdotc(&z__2, i__4, &a[k + 1 + k * a_dim1], 1, &z[k + 1], 1);
      z__1.r = z[i__3].r + z__2.r, z__1.i = z[i__3].i + z__2.i;
      z[i__2].r = z__1.r, z[i__2].i = z__1.i;
    }
    i__2 = k;
    i__3 = k;
    z__1.r = z[i__3].r * 0. - z[i__3].i * -1., z__1.i = z[i__3].r * -1. + 
      z[i__3].i * 0.;
    if ((d__1 = z[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) <= 1.) 
      {
	goto L110;
      }
    i__2 = k;
    i__3 = k;
    z__1.r = z[i__3].r * 0. - z[i__3].i * -1., z__1.i = z[i__3].r * -1. + 
      z[i__3].i * 0.;
    s = 1. / ((d__1 = z[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)));
    blas_zdscal(n, s, &z[1], 1);
  L110:
    l = ipvt[k];
    i__2 = l;
    t.r = z[i__2].r, t.i = z[i__2].i;
    i__2 = l;
    i__3 = k;
    z[i__2].r = z[i__3].r, z[i__2].i = z[i__3].i;
    i__2 = k;
    z[i__2].r = t.r, z[i__2].i = t.i;
    /* L120: */
  }
  s = 1. / blas_dzasum(n, &z[1], 1);
  blas_zdscal(n, s, &z[1], 1);

  ynorm = 1.;

  /* solve l*v = y */

  i__1 = n;
  for (k = 1; k <= i__1; ++k) {
    l = ipvt[k];
    i__2 = l;
    t.r = z[i__2].r, t.i = z[i__2].i;
    i__2 = l;
    i__3 = k;
    z[i__2].r = z[i__3].r, z[i__2].i = z[i__3].i;
    i__2 = k;
    z[i__2].r = t.r, z[i__2].i = t.i;
    if (k < n) {
      i__2 = n - k;
      blas_zaxpy(i__2, &t, &a[k + 1 + k * a_dim1], 1, &z[k + 1], 1);
    }
    i__2 = k;
    i__3 = k;
    z__1.r = z[i__3].r * 0. - z[i__3].i * -1., z__1.i = z[i__3].r * -1. + 
      z[i__3].i * 0.;
    if ((d__1 = z[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) <= 1.) 
      {
	goto L130;
      }
    i__2 = k;
    i__3 = k;
    z__1.r = z[i__3].r * 0. - z[i__3].i * -1., z__1.i = z[i__3].r * -1. + 
      z[i__3].i * 0.;
    s = 1. / ((d__1 = z[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)));
    blas_zdscal(n, s, &z[1], 1);
    ynorm = s * ynorm;
  L130:
    /* L140: */
    ;
  }
  s = 1. / blas_dzasum(n, &z[1], 1);
  blas_zdscal(n, s, &z[1], 1);
  ynorm = s * ynorm;

  /* solve  u*z = v */

  i__1 = n;
  for (kb = 1; kb <= i__1; ++kb) {
    k = n + 1 - kb;
    i__2 = k;
    i__3 = k;
    z__1.r = z[i__3].r * 0. - z[i__3].i * -1., z__1.i = z[i__3].r * -1. + 
      z[i__3].i * 0.;
    i__4 = k + k * a_dim1;
    i__5 = k + k * a_dim1;
    z__2.r = a[i__5].r * 0. - a[i__5].i * -1., z__2.i = a[i__5].r * -1. + 
      a[i__5].i * 0.;
    if ((d__1 = z[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) <= (
								       d__3 = a[i__4].r, abs(d__3)) + (d__4 = z__2.r, abs(d__4))) {
      goto L150;
    }
    i__2 = k + k * a_dim1;
    i__3 = k + k * a_dim1;
    z__1.r = a[i__3].r * 0. - a[i__3].i * -1., z__1.i = a[i__3].r * -1. + 
      a[i__3].i * 0.;
    i__4 = k;
    i__5 = k;
    z__2.r = z[i__5].r * 0. - z[i__5].i * -1., z__2.i = z[i__5].r * -1. + 
      z[i__5].i * 0.;
    s = ((d__1 = a[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2))) / ((
									 d__3 = z[i__4].r, abs(d__3)) + (d__4 = z__2.r, abs(d__4)));
    blas_zdscal(n, s, &z[1], 1);
    ynorm = s * ynorm;
  L150:
    i__2 = k + k * a_dim1;
    i__3 = k + k * a_dim1;
    z__1.r = a[i__3].r * 0. - a[i__3].i * -1., z__1.i = a[i__3].r * -1. + 
      a[i__3].i * 0.;
    if ((d__1 = a[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) != 0.) 
      {
	i__4 = k;
	z_div(&z__2, &z[k], &a[k + k * a_dim1]);
	z[i__4].r = z__2.r, z[i__4].i = z__2.i;
      }
    i__2 = k + k * a_dim1;
    i__3 = k + k * a_dim1;
    z__1.r = a[i__3].r * 0. - a[i__3].i * -1., z__1.i = a[i__3].r * -1. + 
      a[i__3].i * 0.;
    if ((d__1 = a[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) 
      {
	i__4 = k;
	z[i__4].r = 1., z[i__4].i = 0.;
      }
    i__2 = k;
    z__1.r = -z[i__2].r, z__1.i = -z[i__2].i;
    t.r = z__1.r, t.i = z__1.i;
    i__2 = k - 1;
    blas_zaxpy(i__2, &t, &a[k * a_dim1 + 1], 1, &z[1], 1);
    /* L160: */
  }
  /* make znorm = 1.0 */
  s = 1. / blas_dzasum(n, &z[1], 1);
  blas_zdscal(n, s, &z[1], 1);
  ynorm = s * ynorm;

  if (anorm != 0.) {
    *rcond = ynorm / anorm;
  }
  if (anorm == 0.) {
    *rcond = 0.;
  }
  return;
}

VOID linpack_zgedi P7C(dcomplex *, a,
		       int, lda,
		       int, n,
		       int *, ipvt,
		       dcomplex *, det,
		       dcomplex *, work,
		       int, job)
{
  /* System generated locals */
  int a_dim1, a_offset, i__1, i__2, i__3, i__4;
  double d__1, d__2;
  dcomplex z__1, z__2;
  static dcomplex one = {1.,0.};

  /* Local variables */
  int i, j, k, l;
  dcomplex t;
  int kb, kp1, nm1;
  double ten;

  /* zgedi computes the determinant and inverse of a matrix */
  /* using the factors computed by zgeco or zgefa. */

  /* on entry */

  /*    a       complex*16(lda, n) */
  /*            the output from zgeco or zgefa. */

  /*    lda     integer */
  /*            the leading dimension of the array  a . */

  /*    n       integer */
  /*            the order of the matrix  a . */

  /*    ipvt    integer(n) */
  /*            the pivot vector from zgeco or zgefa. */

  /*    work    complex*16(n) */
  /*            work vector.  contents destroyed. */

  /*    job     integer */
  /*            = 11   both determinant and inverse. */
  /*            = 01   inverse only. */
  /*            = 10   determinant only. */

  /* on return */

  /*    a       inverse of original matrix if requested. */
  /*            otherwise unchanged. */

  /*    det     complex*16(2) */
  /*            determinant of original matrix if requested. */
  /*            otherwise not referenced. */
  /*            determinant = det(1) * 10.0**det(2) */
  /*            with  1.0 .le. cabs1(det(1)) .lt. 10.0 */
  /*            or  det(1) .eq. 0.0 . */

  /* error condition */

  /*    a division by zero will occur if the input factor contains */
  /*    a zero on the diagonal and the inverse is requested. */
  /*    it will not occur if the subroutines are called correctly */
  /*    and if zgeco has set rcond .gt. 0.0 or zgefa has set */
  /*    info .eq. 0 . */

  /* linpack. this version dated 08/14/78 . */
  /* cleve moler, university of new mexico, argonne national lab. */

  /* subroutines and functions */

  /* blas zaxpy,zscal,zswap */
  /* fortran dabs,dcmplx,mod */

  /* Parameter adjustments */
  a_dim1 = lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --ipvt;
  --det;
  --work;

  /* compute determinant */

  if (job / 10 == 0) {
    goto L70;
  }
  det[1].r = 1., det[1].i = 0.;
  det[2].r = 0., det[2].i = 0.;
  ten = 10.;
  i__1 = n;
  for (i = 1; i <= i__1; ++i) {
    if (ipvt[i] != i) {
      z__1.r = -det[1].r, z__1.i = -det[1].i;
      det[1].r = z__1.r, det[1].i = z__1.i;
    }
    i__2 = i + i * a_dim1;
    z__1.r = a[i__2].r * det[1].r - a[i__2].i * det[1].i, z__1.i = a[i__2]
      .r * det[1].i + a[i__2].i * det[1].r;
    det[1].r = z__1.r, det[1].i = z__1.i;
    /*    ...exit */
    z__1.r = det[1].r * 0. - det[1].i * -1., z__1.i = det[1].r * -1. + 
      det[1].i * 0.;
    if ((d__1 = det[1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) {
      goto L60;
    }
  L10:
    z__1.r = det[1].r * 0. - det[1].i * -1., z__1.i = det[1].r * -1. + 
      det[1].i * 0.;
    if ((d__1 = det[1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) >= 1.) {
      goto L20;
    }
    z__2.r = ten, z__2.i = 0.;
    z__1.r = z__2.r * det[1].r - z__2.i * det[1].i, z__1.i = z__2.r * det[
									  1].i + z__2.i * det[1].r;
    det[1].r = z__1.r, det[1].i = z__1.i;
    z__1.r = det[2].r - 1., z__1.i = det[2].i + 0.;
    det[2].r = z__1.r, det[2].i = z__1.i;
    goto L10;
  L20:
  L30:
    z__1.r = det[1].r * 0. - det[1].i * -1., z__1.i = det[1].r * -1. + 
      det[1].i * 0.;
    if ((d__1 = det[1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) < ten) {
      goto L40;
    }
    z__2.r = ten, z__2.i = 0.;
    z_div(&z__1, &det[1], &z__2);
    det[1].r = z__1.r, det[1].i = z__1.i;
    z__1.r = det[2].r + 1., z__1.i = det[2].i + 0.;
    det[2].r = z__1.r, det[2].i = z__1.i;
    goto L30;
  L40:
    /* L50: */
    ;
  }
 L60:
 L70:

  /* compute inverse(u) */

  if (job % 10 == 0) {
    goto L150;
  }
  i__1 = n;
  for (k = 1; k <= i__1; ++k) {
    i__2 = k + k * a_dim1;
    z_div(&z__1, &one, &a[k + k * a_dim1]);
    a[i__2].r = z__1.r, a[i__2].i = z__1.i;
    i__2 = k + k * a_dim1;
    z__1.r = -a[i__2].r, z__1.i = -a[i__2].i;
    t.r = z__1.r, t.i = z__1.i;
    i__2 = k - 1;
    blas_zscal(i__2, &t, &a[k * a_dim1 + 1], 1);
    kp1 = k + 1;
    if (n < kp1) {
      goto L90;
    }
    i__2 = n;
    for (j = kp1; j <= i__2; ++j) {
      i__3 = k + j * a_dim1;
      t.r = a[i__3].r, t.i = a[i__3].i;
      i__3 = k + j * a_dim1;
      a[i__3].r = 0., a[i__3].i = 0.;
      blas_zaxpy(k, &t, &a[k * a_dim1 + 1], 1, &a[j * a_dim1 + 1], 1);
      /* L80: */
    }
  L90:
    /* L100: */
    ;
  }

  /*    form inverse(u)*inverse(l) */

  nm1 = n - 1;
  if (nm1 < 1) {
    goto L140;
  }
  i__1 = nm1;
  for (kb = 1; kb <= i__1; ++kb) {
    k = n - kb;
    kp1 = k + 1;
    i__2 = n;
    for (i = kp1; i <= i__2; ++i) {
      i__3 = i;
      i__4 = i + k * a_dim1;
      work[i__3].r = a[i__4].r, work[i__3].i = a[i__4].i;
      i__3 = i + k * a_dim1;
      a[i__3].r = 0., a[i__3].i = 0.;
      /* L110: */
    }
    i__2 = n;
    for (j = kp1; j <= i__2; ++j) {
      i__3 = j;
      t.r = work[i__3].r, t.i = work[i__3].i;
      blas_zaxpy(n, &t, &a[j * a_dim1 + 1], 1, &a[k * a_dim1 + 1], 1);
      /* L120: */
    }
    l = ipvt[k];
    if (l != k) {
      blas_zswap(n, &a[k * a_dim1 + 1], 1, &a[l * a_dim1 + 1], 1);
    }
    /* L130: */
  }
 L140:
 L150:
  return;
}

VOID linpack_zgefa P5C(dcomplex *, a,
		       int, lda,
		       int, n,
		       int *, ipvt,
		       int *, info)
{
  /* System generated locals */
  int a_dim1, a_offset, i__1, i__2, i__3, i__4;
  double d__1, d__2;
  dcomplex z__1;
  static dcomplex negone = {-1.,0.};

  /* Local variables */
  int j, k, l;
  dcomplex t;
  int kp1, nm1;

  /* zgefa factors a complex*16 matrix by gaussian elimination. */

  /* zgefa is usually called by zgeco, but it can be called */
  /* directly with a saving in time if  rcond  is not needed. */
  /* (time for zgeco) = (1 + 9/n)*(time for zgefa) . */

  /* on entry */

  /*    a       complex*16(lda, n) */
  /*            the matrix to be factored. */

  /*    lda     integer */
  /*            the leading dimension of the array  a . */

  /*    n       integer */
  /*            the order of the matrix  a . */

  /* on return */

  /*    a       an upper triangular matrix and the multipliers */
  /*            which were used to obtain it. */
  /*            the factorization can be written  a = l*u  where */
  /*            l  is a product of permutation and unit lower */
  /*            triangular matrices and  u  is upper triangular. */

  /*    ipvt    integer(n) */
  /*            an integer vector of pivot indices. */

  /*    info    integer */
  /*            = 0  normal value. */
  /*            = k  if  u(k,k) .eq. 0.0 .  this is not an error */
  /*                 condition for this subroutine, but it does */
  /*                 indicate that zgesl or zgedi will divide by zero */
  /*                 if called.  use  rcond  in zgeco for a reliable */
  /*                 indication of singularity. */

  /* linpack. this version dated 08/14/78 . */
  /* cleve moler, university of new mexico, argonne national lab. */

  /* subroutines and functions */

  /* blas zaxpy,zscal,izamax */
  /* fortran dabs */

  /* Parameter adjustments */
  a_dim1 = lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --ipvt;

  /* gaussian elimination with partial pivoting */

  *info = 0;
  nm1 = n - 1;
  if (nm1 < 1) {
    goto L70;
  }
  i__1 = nm1;
  for (k = 1; k <= i__1; ++k) {
    kp1 = k + 1;

    /*    find l = pivot index */

    i__2 = n - k + 1;
    l = blas_izamax(i__2, &a[k + k * a_dim1], 1) + k;
    ipvt[k] = l;

    /*    zero pivot implies this column already triangularized */

    i__2 = l + k * a_dim1;
    i__3 = l + k * a_dim1;
    z__1.r = a[i__3].r * 0. - a[i__3].i * -1., z__1.i = a[i__3].r * -1. + 
      a[i__3].i * 0.;
    if ((d__1 = a[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) 
      {
	goto L40;
      }

    /*       interchange if necessary */

    if (l == k) {
      goto L10;
    }
    i__2 = l + k * a_dim1;
    t.r = a[i__2].r, t.i = a[i__2].i;
    i__2 = l + k * a_dim1;
    i__3 = k + k * a_dim1;
    a[i__2].r = a[i__3].r, a[i__2].i = a[i__3].i;
    i__2 = k + k * a_dim1;
    a[i__2].r = t.r, a[i__2].i = t.i;
  L10:

    /*       compute multipliers */

    z_div(&z__1, &negone, &a[k + k * a_dim1]);
    t.r = z__1.r, t.i = z__1.i;
    i__2 = n - k;
    blas_zscal(i__2, &t, &a[k + 1 + k * a_dim1], 1);

    /*       row elimination with column indexing */

    i__2 = n;
    for (j = kp1; j <= i__2; ++j) {
      i__3 = l + j * a_dim1;
      t.r = a[i__3].r, t.i = a[i__3].i;
      if (l == k) {
	goto L20;
      }
      i__3 = l + j * a_dim1;
      i__4 = k + j * a_dim1;
      a[i__3].r = a[i__4].r, a[i__3].i = a[i__4].i;
      i__3 = k + j * a_dim1;
      a[i__3].r = t.r, a[i__3].i = t.i;
    L20:
      i__3 = n - k;
      blas_zaxpy(i__3, &t, &a[k + 1 + k * a_dim1], 1, &a[k + 1 + j * 
							 a_dim1], 1);
      /* L30: */
    }
    goto L50;
  L40:
    *info = k;
  L50:
    /* L60: */
    ;
  }
 L70:
  ipvt[n] = n;
  i__1 = n + n * a_dim1;
  i__2 = n + n * a_dim1;
  z__1.r = a[i__2].r * 0. - a[i__2].i * -1., z__1.i = a[i__2].r * -1. + a[
									  i__2].i * 0.;
  if ((d__1 = a[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) {
    *info = n;
  }
  return;
}

VOID linpack_zgesl P6C(dcomplex *, a,
		       int, lda,
		       int, n,
		       int *, ipvt,
		       dcomplex *, b,
		       int, job)
{
  /* System generated locals */
  int a_dim1, a_offset, i__1, i__2, i__3, i__4;
  dcomplex z__1, z__2, z__3;

  /* Local variables */
  int k, l;
  dcomplex t;
  int kb, nm1;

  /* zgesl solves the complex*16 system */
  /* a * x = b  or  ctrans(a) * x = b */
  /* using the factors computed by zgeco or zgefa. */

  /* on entry */

  /*    a       complex*16(lda, n) */
  /*            the output from zgeco or zgefa. */

  /*    lda     integer */
  /*            the leading dimension of the array  a . */

  /*    n       integer */
  /*            the order of the matrix  a . */

  /*    ipvt    integer(n) */
  /*            the pivot vector from zgeco or zgefa. */

  /*    b       complex*16(n) */
  /*            the right hand side vector. */

  /*    job     integer */
  /*            = 0         to solve  a*x = b , */
  /*            = nonzero   to solve  ctrans(a)*x = b  where */
  /*                        ctrans(a)  is the conjugate transpose. */

  /* on return */

  /*    b       the solution vector  x . */

  /* error condition */

  /*    a division by zero will occur if the input factor contains a */
  /*    zero on the diagonal.  technically this indicates singularity */
  /*    but it is often caused by improper arguments or improper */
  /*    setting of lda .  it will not occur if the subroutines are */
  /*    called correctly and if zgeco has set rcond .gt. 0.0 */
  /*    or zgefa has set info .eq. 0 . */

  /* to compute  inverse(a) * c  where  c  is a matrix */
  /* with  p  columns */
  /*       call zgeco(a,lda,n,ipvt,rcond,z) */
  /*       if (rcond is too small) go to ... */
  /*       do 10 j = 1, p */
  /*          call zgesl(a,lda,n,ipvt,c(1,j),0) */
  /*    10 continue */

  /* linpack. this version dated 08/14/78 . */
  /* cleve moler, university of new mexico, argonne national lab. */

  /* subroutines and functions */

  /* blas zaxpy,zdotc */
  /* fortran dconjg */

  /* Parameter adjustments */
  a_dim1 = lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --ipvt;
  --b;

  nm1 = n - 1;
  if (job != 0) {
    goto L50;
  }

  /*    job = 0 , solve  a * x = b */
  /*    first solve  l*y = b */

  if (nm1 < 1) {
    goto L30;
  }
  i__1 = nm1;
  for (k = 1; k <= i__1; ++k) {
    l = ipvt[k];
    i__2 = l;
    t.r = b[i__2].r, t.i = b[i__2].i;
    if (l == k) {
      goto L10;
    }
    i__2 = l;
    i__3 = k;
    b[i__2].r = b[i__3].r, b[i__2].i = b[i__3].i;
    i__2 = k;
    b[i__2].r = t.r, b[i__2].i = t.i;
  L10:
    i__2 = n - k;
    blas_zaxpy(i__2, &t, &a[k + 1 + k * a_dim1], 1, &b[k + 1], 1);
    /* L20: */
  }
 L30:

  /*    now solve  u*x = y */

  i__1 = n;
  for (kb = 1; kb <= i__1; ++kb) {
    k = n + 1 - kb;
    i__2 = k;
    z_div(&z__1, &b[k], &a[k + k * a_dim1]);
    b[i__2].r = z__1.r, b[i__2].i = z__1.i;
    i__2 = k;
    z__1.r = -b[i__2].r, z__1.i = -b[i__2].i;
    t.r = z__1.r, t.i = z__1.i;
    i__2 = k - 1;
    blas_zaxpy(i__2, &t, &a[k * a_dim1 + 1], 1, &b[1], 1);
    /* L40: */
  }
  goto L100;
 L50:

  /*    job = nonzero, solve  ctrans(a) * x = b */
  /*    first solve  ctrans(u)*y = b */

  i__1 = n;
  for (k = 1; k <= i__1; ++k) {
    i__2 = k - 1;
    blas_zdotc(&z__1, i__2, &a[k * a_dim1 + 1], 1, &b[1], 1);
    t.r = z__1.r, t.i = z__1.i;
    i__2 = k;
    i__3 = k;
    z__2.r = b[i__3].r - t.r, z__2.i = b[i__3].i - t.i;
    d_cnjg(&z__3, &a[k + k * a_dim1]);
    z_div(&z__1, &z__2, &z__3);
    b[i__2].r = z__1.r, b[i__2].i = z__1.i;
    /* L60: */
  }

  /*    now solve ctrans(l)*x = y */

  if (nm1 < 1) {
    goto L90;
  }
  i__1 = nm1;
  for (kb = 1; kb <= i__1; ++kb) {
    k = n - kb;
    i__2 = k;
    i__3 = k;
    i__4 = n - k;
    blas_zdotc(&z__2, i__4, &a[k + 1 + k * a_dim1], 1, &b[k + 1], 1);
    z__1.r = b[i__3].r + z__2.r, z__1.i = b[i__3].i + z__2.i;
    b[i__2].r = z__1.r, b[i__2].i = z__1.i;
    l = ipvt[k];
    if (l == k) {
      goto L70;
    }
    i__2 = l;
    t.r = b[i__2].r, t.i = b[i__2].i;
    i__2 = l;
    i__3 = k;
    b[i__2].r = b[i__3].r, b[i__2].i = b[i__3].i;
    i__2 = k;
    b[i__2].r = t.r, b[i__2].i = t.i;
  L70:
    /* L80: */
    ;
  }
 L90:
 L100:
  return;
}
