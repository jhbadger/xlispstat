/* QR Decomposition from LINPACK */

#include "linalg.h"

VOID linpack_dqrdc P8C(double *, x,
		       int, ldx,
		       int, n,
		       int, p,
		       double *, qraux,
		       int *, jpvt,
		       double *, work,
		       int, job)
{
  /* System generated locals */
  int x_dim1, x_offset, i__1, i__2, i__3;
  double d__1, d__2;

  /* Local variables */
  int negj;
  int maxj;
  int j, l;
  double t;
  int swapj;
  double nrmxl;
  int jj, jp, pl, pu;
  double tt, maxnrm;
  int lp1, lup;

  /* dqrdc uses householder transformations to compute the qr */
  /* factorization of an n by p matrix x.  column pivoting */
  /* based on the 2-norms of the reduced columns may be */
  /* performed at the users option. */

  /* on entry */

  /*    x       double precision(ldx,p), where ldx .ge. n. */
  /*            x contains the matrix whose decomposition is to be */
  /*            computed. */

  /*    ldx     integer. */
  /*            ldx is the leading dimension of the array x. */

  /*    n       integer. */
  /*            n is the number of rows of the matrix x. */

  /*    p       integer. */
  /*            p is the number of columns of the matrix x. */

  /*    jpvt    integer(p). */
  /*            jpvt contains integers that control the selection */
  /*            of the pivot columns.  the k-th column x(k) of x */
  /*            is placed in one of three classes according to the */
  /*            value of jpvt(k). */

  /*               if jpvt(k) .gt. 0, then x(k) is an initial */
  /*                                  column. */

  /*               if jpvt(k) .eq. 0, then x(k) is a free column. */

  /*               if jpvt(k) .lt. 0, then x(k) is a final column. */

  /*            before the decomposition is computed, initial columns */
  /*            are moved to the beginning of the array x and final */
  /*            columns to the end.  both initial and final columns */
  /*            are frozen in place during the computation and only */
  /*            free columns are moved.  at the k-th stage of the */
  /*            reduction, if x(k) is occupied by a free column */
  /*            it is interchanged with the free column of largest */
  /*            reduced norm.  jpvt is not referenced if */
  /*            job .eq. 0. */

  /*    work    double precision(p). */
  /*            work is a work array.  work is not referenced if */
  /*            job .eq. 0. */

  /*    job     integer. */
  /*            job is an integer that initiates column pivoting. */
  /*            if job .eq. 0, no pivoting is done. */
  /*            if job .ne. 0, pivoting is done. */

  /* on return */

  /*    x       x contains in its upper triangle the upper */
  /*            triangular matrix r of the qr factorization. */
  /*            below its diagonal x contains information from */
  /*            which the orthogonal part of the decomposition */
  /*            can be recovered.  note that if pivoting has */
  /*            been requested, the decomposition is not that */
  /*            of the original matrix x but that of x */
  /*            with its columns permuted as described by jpvt. */

  /*    qraux   double precision(p). */
  /*            qraux contains further information required to recover */
  /*            the orthogonal part of the decomposition. */

  /*    jpvt    jpvt(k) contains the index of the column of the */
  /*            original matrix that has been interchanged into */
  /*            the k-th column, if pivoting was requested. */

  /* linpack. this version dated 08/14/78 . */
  /* g.w. stewart, university of maryland, argonne national lab. */

  /* dqrdc uses the following functions and subprograms. */

  /* blas daxpy,ddot,dscal,dswap,dnrm2 */
  /* fortran dabs,dmax1,min0,dsqrt */

  /* Parameter adjustments */
  x_dim1 = ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  --qraux;
  --jpvt;
  --work;

  /* Function Body */
  pl = 1;
  pu = 0;
  if (job == 0) {
    goto L60;
  }

  /*    pivoting has been requested.  rearrange the columns */
  /*    according to jpvt. */

  i__1 = p;
  for (j = 1; j <= i__1; ++j) {
    swapj = jpvt[j] > 0;
    negj = jpvt[j] < 0;
    jpvt[j] = j;
    if (negj) {
      jpvt[j] = -j;
    }
    if (! swapj) {
      goto L10;
    }
    if (j != pl) {
      blas_dswap(n, &x[pl * x_dim1 + 1], 1, &x[j * x_dim1 + 1], 1);
    }
    jpvt[j] = jpvt[pl];
    jpvt[pl] = j;
    ++pl;
  L10:
    /* L20: */
    ;
  }
  pu = p;
  i__1 = p;
  for (jj = 1; jj <= i__1; ++jj) {
    j = p - jj + 1;
    if (jpvt[j] >= 0) {
      goto L40;
    }
    jpvt[j] = -jpvt[j];
    if (j == pu) {
      goto L30;
    }
    blas_dswap(n, &x[pu * x_dim1 + 1], 1, &x[j * x_dim1 + 1], 1);
    jp = jpvt[pu];
    jpvt[pu] = jpvt[j];
    jpvt[j] = jp;
  L30:
    --pu;
  L40:
    /* L50: */
    ;
  }
 L60:

  /* compute the norms of the free columns. */

  if (pu < pl) {
    goto L80;
  }
  i__1 = pu;
  for (j = pl; j <= i__1; ++j) {
    qraux[j] = blas_dnrm2(n, &x[j * x_dim1 + 1], 1);
    work[j] = qraux[j];
    /* L70: */
  }
 L80:

  /* perform the householder reduction of x. */

  lup = min(n,p);
  i__1 = lup;
  for (l = 1; l <= i__1; ++l) {
    if (l < pl || l >= pu) {
      goto L120;
    }

    /*       locate the column of largest norm and bring it */
    /*       into the pivot position. */

    maxnrm = 0.;
    maxj = l;
    i__2 = pu;
    for (j = l; j <= i__2; ++j) {
      if (qraux[j] <= maxnrm) {
	goto L90;
      }
      maxnrm = qraux[j];
      maxj = j;
    L90:
      /* L100: */
      ;
    }
    if (maxj == l) {
      goto L110;
    }
    blas_dswap(n, &x[l * x_dim1 + 1], 1, &x[maxj * x_dim1 + 1], 1);
    qraux[maxj] = qraux[l];
    work[maxj] = work[l];
    jp = jpvt[maxj];
    jpvt[maxj] = jpvt[l];
    jpvt[l] = jp;
  L110:
  L120:
    qraux[l] = 0.;
    if (l == n) {
      goto L190;
    }

    /*       compute the householder transformation for column l. */

    i__2 = n - l + 1;
    nrmxl = blas_dnrm2(i__2, &x[l + l * x_dim1], 1);
    if (nrmxl == 0.) {
      goto L180;
    }
    if (x[l + l * x_dim1] != 0.) {
      nrmxl = d_sign(&nrmxl, &x[l + l * x_dim1]);
    }
    i__2 = n - l + 1;
    d__1 = 1. / nrmxl;
    blas_dscal(i__2, d__1, &x[l + l * x_dim1], 1);
    x[l + l * x_dim1] += 1.;

    /*          apply the transformation to the remaining columns, */
    /*          updating the norms. */

    lp1 = l + 1;
    if (p < lp1) {
      goto L170;
    }
    i__2 = p;
    for (j = lp1; j <= i__2; ++j) {
      i__3 = n - l + 1;
      t = -blas_ddot(i__3, &x[l + l * x_dim1], 1, &x[l + j * x_dim1],
		     1) / x[l + l * x_dim1];
      i__3 = n - l + 1;
      blas_daxpy(i__3, t, &x[l + l * x_dim1], 1, &x[l + j * x_dim1], 1);
      if (j < pl || j > pu) {
	goto L150;
      }
      if (qraux[j] == 0.) {
	goto L150;
      }
      /* Computing 2nd power */
      d__2 = (d__1 = x[l + j * x_dim1], abs(d__1)) / qraux[j];
      tt = 1. - d__2 * d__2;
      tt = max(tt,0.);
      t = tt;
      /* Computing 2nd power */
      d__1 = qraux[j] / work[j];
      tt = tt * .05 * (d__1 * d__1) + 1.;
      if (tt == 1.) {
	goto L130;
      }
      qraux[j] *= sqrt(t);
      goto L140;
    L130:
      i__3 = n - l;
      qraux[j] = blas_dnrm2(i__3, &x[l + 1 + j * x_dim1], 1);
      work[j] = qraux[j];
    L140:
    L150:
      /* L160: */
      ;
    }
  L170:

    /*          save the transformation. */

    qraux[l] = x[l + l * x_dim1];
    x[l + l * x_dim1] = -nrmxl;
  L180:
  L190:
    /* L200: */
    ;
  }
  return;
}

VOID linpack_dqrsl P13C(double *, x,
			int, ldx,
			int, n,
			int, k,
			double *, qraux,
			double *, y,
			double *, qy,
			double *, qty,
			double *, b,
			double *, rsd,
			double *, xb,
			int, job, 
			int *, info)
{
  /* System generated locals */
  integer x_dim1, x_offset, i__1, i__2;

  /* Local variables */
  double temp;
  int cqty;
  int i, j;
  double t;
  int cb;
  int jj;
  int cr;
  int ju, kp1;
  int cxb, cqy;

  /* dqrsl applies the output of dqrdc to compute coordinate */
  /* transformations, projections, and least squares solutions. */
  /* for k .le. min(n,p), let xk be the matrix */

  /*        xk = (x(jpvt(1)),x(jpvt(2)), ... ,x(jpvt(k))) */

  /* formed from columnns jpvt(1), ... ,jpvt(k) of the original */
  /* n x p matrix x that was input to dqrdc (if no pivoting was */
  /* done, xk consists of the first k columns of x in their */
  /* original order).  dqrdc produces a factored orthogonal matrix q */
  /* and an upper triangular matrix r such that */

  /*          xk = q * (r) */
  /*                   (0) */

  /* this information is contained in coded form in the arrays */
  /* x and qraux. */

  /* on entry */

  /*    x      double precision(ldx,p). */
  /*           x contains the output of dqrdc. */

  /*    ldx    integer. */
  /*           ldx is the leading dimension of the array x. */

  /*    n      integer. */
  /*           n is the number of rows of the matrix xk.  it must */
  /*           have the same value as n in dqrdc. */

  /*    k      integer. */
  /*           k is the number of columns of the matrix xk.  k */
  /*           must nnot be greater than min(n,p), where p is the */
  /*           same as in the calling sequence to dqrdc. */

  /*    qraux  double precision(p). */
  /*           qraux contains the auxiliary output from dqrdc. */

  /*    y      double precision(n) */
  /*           y contains an n-vector that is to be manipulated */
  /*           by dqrsl. */

  /*    job    integer. */
  /*           job specifies what is to be computed.  job has */
  /*           the decimal expansion abcde, with the following */
  /*           meaning. */

  /*                if a.ne.0, compute qy. */
  /*                if b,c,d, or e .ne. 0, compute qty. */
  /*                if c.ne.0, compute b. */
  /*                if d.ne.0, compute rsd. */
  /*                if e.ne.0, compute xb. */

  /*           note that a request to compute b, rsd, or xb */
  /*           automatically triggers the computation of qty, for */
  /*           which an array must be provided in the calling */
  /*           sequence. */

  /* on return */

  /*    qy     double precision(n). */
  /*           qy conntains q*y, if its computation has been */
  /*           requested. */

  /*    qty    double precision(n). */
  /*           qty contains trans(q)*y, if its computation has */
  /*           been requested.  here trans(q) is the */
  /*           transpose of the matrix q. */

  /*    b      double precision(k) */
  /*           b contains the solution of the least squares problem */

  /*                minimize norm2(y - xk*b), */

  /*           if its computation has been requested.  (note that */
  /*           if pivoting was requested in dqrdc, the j-th */
  /*           component of b will be associated with column jpvt(j) */
  /*           of the original matrix x that was input into dqrdc.) */

  /*    rsd    double precision(n). */
  /*           rsd contains the least squares residual y - xk*b, */
  /*           if its computation has been requested.  rsd is */
  /*           also the orthogonal projection of y onto the */
  /*           orthogonal complement of the column space of xk. */

  /*    xb     double precision(n). */
  /*           xb contains the least squares approximation xk*b, */
  /*           if its computation has been requested.  xb is also */
  /*           the orthogonal projection of y onto the column space */
  /*           of x. */

  /*    info   integer. */
  /*           info is zero unless the computation of b has */
  /*           been requested and r is exactly singular.  in */
  /*           this case, info is the index of the first zero */
  /*           diagonal element of r and b is left unaltered. */

  /* the parameters qy, qty, b, rsd, and xb are not referenced */
  /* if their computation is not requested and in this case */
  /* can be replaced by dummy variables in the calling program. */
  /* to save storage, the user may in some cases use the same */
  /* array for different parameters in the calling sequence.  a */
  /* frequently occuring example is when one wishes to compute */
  /* any of b, rsd, or xb and does not need y or qty.  in this */
  /* case one may identify y, qty, and one of b, rsd, or xb, while */
  /* providing separate arrays for anything else that is to be */
  /* computed.  thus the calling sequence */

  /*      call dqrsl(x,ldx,n,k,qraux,y,dum,y,b,y,dum,110,info) */

  /* will result in the computation of b and rsd, with rsd */
  /* overwriting y.  more generally, each item in the following */
  /* list contains groups of permissible identifications for */
  /* a single callinng sequence. */

  /*      1. (y,qty,b) (rsd) (xb) (qy) */

  /*      2. (y,qty,rsd) (b) (xb) (qy) */

  /*      3. (y,qty,xb) (b) (rsd) (qy) */

  /*      4. (y,qy) (qty,b) (rsd) (xb) */

  /*      5. (y,qy) (qty,rsd) (b) (xb) */

  /*      6. (y,qy) (qty,xb) (b) (rsd) */

  /* in any group the value returned in the array allocated to */
  /* the group corresponds to the last member of the group. */

  /* linpack. this version dated 08/14/78 . */
  /* g.w. stewart, university of maryland, argonne national lab. */

  /* dqrsl uses the following functions and subprograms. */

  /* blas daxpy,dcopy,ddot */
  /* fortran dabs,min0,mod */

  /* Parameter adjustments */
  x_dim1 = ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  --qraux;
  --y;
  --qy;
  --qty;
  --b;
  --rsd;
  --xb;

  /* set info flag. */

  *info = 0;

  /* determine what is to be computed. */

  cqy = job / 10000 != 0;
  cqty = job % 10000 != 0;
  cb = job % 1000 / 100 != 0;
  cr = job % 100 / 10 != 0;
  cxb = job % 10 != 0;
  /* Computing MIN */
  i__1 = k, i__2 = n - 1;
  ju = min(i__1,i__2);

  /* special action when n=1. */

  if (ju != 0) {
    goto L40;
  }
  if (cqy) {
    qy[1] = y[1];
  }
  if (cqty) {
    qty[1] = y[1];
  }
  if (cxb) {
    xb[1] = y[1];
  }
  if (! cb) {
    goto L30;
  }
  if (x[x_dim1 + 1] != 0.) {
    goto L10;
  }
  *info = 1;
  goto L20;
 L10:
  b[1] = y[1] / x[x_dim1 + 1];
 L20:
 L30:
  if (cr) {
    rsd[1] = 0.;
  }
  goto L250;
 L40:

  /*    set up to compute qy or qty. */

  if (cqy) {
    blas_dcopy(n, &y[1], 1, &qy[1], 1);
  }
  if (cqty) {
    blas_dcopy(n, &y[1], 1, &qty[1], 1);
  }
  if (! cqy) {
    goto L70;
  }

  /*       compute qy. */

  i__1 = ju;
  for (jj = 1; jj <= i__1; ++jj) {
    j = ju - jj + 1;
    if (qraux[j] == 0.) {
      goto L50;
    }
    temp = x[j + j * x_dim1];
    x[j + j * x_dim1] = qraux[j];
    i__2 = n - j + 1;
    t = -blas_ddot(i__2, &x[j + j * x_dim1], 1, &qy[j], 1) / x[j + j 
							       * x_dim1];
    i__2 = n - j + 1;
    blas_daxpy(i__2, t, &x[j + j * x_dim1], 1, &qy[j], 1);
    x[j + j * x_dim1] = temp;
  L50:
    /* L60: */
    ;
  }
 L70:
  if (! cqty) {
    goto L100;
  }

  /*       compute trans(q)*y. */

  i__1 = ju;
  for (j = 1; j <= i__1; ++j) {
    if (qraux[j] == 0.) {
      goto L80;
    }
    temp = x[j + j * x_dim1];
    x[j + j * x_dim1] = qraux[j];
    i__2 = n - j + 1;
    t = -blas_ddot(i__2, &x[j + j * x_dim1], 1, &qty[j], 1) / x[j + 
								j * x_dim1];
    i__2 = n - j + 1;
    blas_daxpy(i__2, t, &x[j + j * x_dim1], 1, &qty[j], 1);
    x[j + j * x_dim1] = temp;
  L80:
    /* L90: */
    ;
  }
 L100:

  /*    set up to compute b, rsd, or xb. */

  if (cb) {
    blas_dcopy(k, &qty[1], 1, &b[1], 1);
  }
  kp1 = k + 1;
  if (cxb) {
    blas_dcopy(k, &qty[1], 1, &xb[1], 1);
  }
  if (cr && k < n) {
    i__1 = n - k;
    blas_dcopy(i__1, &qty[kp1], 1, &rsd[kp1], 1);
  }
  if (! cxb || kp1 > n) {
    goto L120;
  }
  i__1 = n;
  for (i = kp1; i <= i__1; ++i) {
    xb[i] = 0.;
    /* L110: */
  }
 L120:
  if (! cr) {
    goto L140;
  }
  i__1 = k;
  for (i = 1; i <= i__1; ++i) {
    rsd[i] = 0.;
    /* L130: */
  }
 L140:
  if (! cb) {
    goto L190;
  }

  /*       compute b. */

  i__1 = k;
  for (jj = 1; jj <= i__1; ++jj) {
    j = k - jj + 1;
    if (x[j + j * x_dim1] != 0.) {
      goto L150;
    }
    *info = j;
    /*       ......exit */
    goto L180;
  L150:
    b[j] /= x[j + j * x_dim1];
    if (j == 1) {
      goto L160;
    }
    t = -b[j];
    i__2 = j - 1;
    blas_daxpy(i__2, t, &x[j * x_dim1 + 1], 1, &b[1], 1);
  L160:
    /* L170: */
    ;
  }
 L180:
 L190:
  if (! cr && ! cxb) {
    goto L240;
  }

  /*       compute rsd or xb as required. */

  i__1 = ju;
  for (jj = 1; jj <= i__1; ++jj) {
    j = ju - jj + 1;
    if (qraux[j] == 0.) {
      goto L220;
    }
    temp = x[j + j * x_dim1];
    x[j + j * x_dim1] = qraux[j];
    if (! cr) {
      goto L200;
    }
    i__2 = n - j + 1;
    t = -blas_ddot(i__2, &x[j + j * x_dim1], 1, &rsd[j], 1) / x[j + 
								j * x_dim1];
    i__2 = n - j + 1;
    blas_daxpy(i__2, t, &x[j + j * x_dim1], 1, &rsd[j], 1);
  L200:
    if (! cxb) {
      goto L210;
    }
    i__2 = n - j + 1;
    t = -blas_ddot(i__2, &x[j + j * x_dim1], 1, &xb[j], 1) / x[j + j 
							       * x_dim1];
    i__2 = n - j + 1;
    blas_daxpy(i__2, t, &x[j + j * x_dim1], 1, &xb[j], 1);
  L210:
    x[j + j * x_dim1] = temp;
  L220:
    /* L230: */
    ;
  }
 L240:
 L250:
  return;
}

VOID linpack_zqrdc P8C(dcomplex *, x,
		       int, ldx,
		       int, n,
		       int, p,
		       dcomplex *, qraux,
		       int *, jpvt,
		       dcomplex *, work,
		       int, job)
{
  /* System generated locals */
  int x_dim1, x_offset, i__1, i__2, i__3, i__4;
  double d__1, d__2, d__3, d__4;
  dcomplex z__1, z__2, z__3;
  static dcomplex c_b28 = {1.,0.};

  /* Local variables */
  int negj;
  int maxj, j, l;
  dcomplex t;
  int swapj;
  dcomplex nrmxl;
  int jj, jp, pl, pu;
  double tt;
  double maxnrm;
  int lp1, lup;


  /*     zqrdc uses householder transformations to compute the qr */
  /*     factorization of an n by p matrix x.  column pivoting */
  /*     based on the 2-norms of the reduced columns may be */
  /*     performed at the users option. */

  /*     on entry */

  /*        x       complex*16(ldx,p), where ldx .ge. n. */
  /*                x contains the matrix whose decomposition is to be */
  /*                computed. */

  /*        ldx     integer. */
  /*                ldx is the leading dimension of the array x. */

  /*        n       integer. */
  /*                n is the number of rows of the matrix x. */

  /*        p       integer. */
  /*                p is the number of columns of the matrix x. */

  /*        jpvt    integer(p). */
  /*                jpvt contains integers that control the selection */
  /*                of the pivot columns.  the k-th column x(k) of x */
  /*                is placed in one of three classes according to the */
  /*                value of jpvt(k). */

  /*                   if jpvt(k) .gt. 0, then x(k) is an initial */
  /*                                      column. */

  /*                   if jpvt(k) .eq. 0, then x(k) is a free column. */

  /*                   if jpvt(k) .lt. 0, then x(k) is a final column. */

  /*                before the decomposition is computed, initial columns */
  /*                are moved to the beginning of the array x and final */
  /*                columns to the end.  both initial and final columns */
  /*                are frozen in place during the computation and only */
  /*                free columns are moved.  at the k-th stage of the */
  /*                reduction, if x(k) is occupied by a free column */
  /*                it is interchanged with the free column of largest */
  /*                reduced norm.  jpvt is not referenced if */
  /*                job .eq. 0. */

  /*        work    complex*16(p). */
  /*                work is a work array.  work is not referenced if */
  /*                job .eq. 0. */

  /*        job     integer. */
  /*                job is an integer that initiates column pivoting. */
  /*                if job .eq. 0, no pivoting is done. */
  /*                if job .ne. 0, pivoting is done. */

  /*     on return */

  /*        x       x contains in its upper triangle the upper */
  /*                triangular matrix r of the qr factorization. */
  /*                below its diagonal x contains information from */
  /*                which the unitary part of the decomposition */
  /*                can be recovered.  note that if pivoting has */
  /*                been requested, the decomposition is not that */
  /*                of the original matrix x but that of x */
  /*                with its columns permuted as described by jpvt. */

  /*        qraux   complex*16(p). */
  /*                qraux contains further information required to recover 
   */
  /*                the unitary part of the decomposition. */

  /*        jpvt    jpvt(k) contains the index of the column of the */
  /*                original matrix that has been interchanged into */
  /*                the k-th column, if pivoting was requested. */

  /*     linpack. this version dated 08/14/78 . */
  /*     g.w. stewart, university of maryland, argonne national lab. */

  /*     zqrdc uses the following functions and subprograms. */

  /*     blas zaxpy,zdotc,zscal,zswap,dznrm2 */
  /*     fortran dabs,dmax1,cdabs,dcmplx,cdsqrt,min0 */

  /*     internal variables */



  /* Parameter adjustments */
  x_dim1 = ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  --qraux;
  --jpvt;
  --work;

  /* Function Body */
  pl = 1;
  pu = 0;
  if (job == 0) {
    goto L60;
  }

  /*        pivoting has been requested.  rearrange the columns */
  /*        according to jpvt. */

  i__1 = p;
  for (j = 1; j <= i__1; ++j) {
    swapj = jpvt[j] > 0;
    negj = jpvt[j] < 0;
    jpvt[j] = j;
    if (negj) {
      jpvt[j] = -j;
    }
    if (! swapj) {
      goto L10;
    }
    if (j != pl) {
      blas_zswap(n, &x[pl * x_dim1 + 1], 1, &x[j * x_dim1 + 1], 1);
    }
    jpvt[j] = jpvt[pl];
    jpvt[pl] = j;
    ++pl;
  L10:
    /* L20: */
    ;
  }
  pu = p;
  i__1 = p;
  for (jj = 1; jj <= i__1; ++jj) {
    j = p - jj + 1;
    if (jpvt[j] >= 0) {
      goto L40;
    }
    jpvt[j] = -jpvt[j];
    if (j == pu) {
      goto L30;
    }
    blas_zswap(n, &x[pu * x_dim1 + 1], 1, &x[j * x_dim1 + 1], 1);
    jp = jpvt[pu];
    jpvt[pu] = jpvt[j];
    jpvt[j] = jp;
  L30:
    --pu;
  L40:
    /* L50: */
    ;
  }
 L60:

  /*     compute the norms of the free columns. */

  if (pu < pl) {
    goto L80;
  }
  i__1 = pu;
  for (j = pl; j <= i__1; ++j) {
    i__2 = j;
    d__1 = blas_dznrm2(n, &x[j * x_dim1 + 1], 1);
    z__1.r = d__1, z__1.i = 0.;
    qraux[i__2].r = z__1.r, qraux[i__2].i = z__1.i;
    i__2 = j;
    i__3 = j;
    work[i__2].r = qraux[i__3].r, work[i__2].i = qraux[i__3].i;
    /* L70: */
  }
 L80:

  /*     perform the householder reduction of x. */

  lup = min(n,p);
  i__1 = lup;
  for (l = 1; l <= i__1; ++l) {
    if (l < pl || l >= pu) {
      goto L120;
    }

    /*           locate the column of largest norm and bring it */
    /*           into the pivot position. */

    maxnrm = 0.;
    maxj = l;
    i__2 = pu;
    for (j = l; j <= i__2; ++j) {
      i__3 = j;
      if (qraux[i__3].r <= maxnrm) {
	goto L90;
      }
      i__3 = j;
      maxnrm = qraux[i__3].r;
      maxj = j;
    L90:
      /* L100: */
      ;
    }
    if (maxj == l) {
      goto L110;
    }
    blas_zswap(n, &x[l * x_dim1 + 1], 1, &x[maxj * x_dim1 + 1], 1);
    i__2 = maxj;
    i__3 = l;
    qraux[i__2].r = qraux[i__3].r, qraux[i__2].i = qraux[i__3].i;
    i__2 = maxj;
    i__3 = l;
    work[i__2].r = work[i__3].r, work[i__2].i = work[i__3].i;
    jp = jpvt[maxj];
    jpvt[maxj] = jpvt[l];
    jpvt[l] = jp;
  L110:
  L120:
    i__2 = l;
    qraux[i__2].r = 0., qraux[i__2].i = 0.;
    if (l == n) {
      goto L190;
    }

    /*           compute the householder transformation for column l. */

    i__2 = n - l + 1;
    d__1 = blas_dznrm2(i__2, &x[l + l * x_dim1], 1);
    z__1.r = d__1, z__1.i = 0.;
    nrmxl.r = z__1.r, nrmxl.i = z__1.i;
    z__1.r = nrmxl.r * 0. - nrmxl.i * -1., z__1.i = nrmxl.r * -1. + 
      nrmxl.i * 0.;
    if ((d__1 = nrmxl.r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) {
      goto L180;
    }
    i__2 = l + l * x_dim1;
    i__3 = l + l * x_dim1;
    z__1.r = x[i__3].r * 0. - x[i__3].i * -1., z__1.i = x[i__3].r * -1. + 
      x[i__3].i * 0.;
    if ((d__1 = x[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) != 0.) 
      {
	d__3 = z_abs(&nrmxl);
	i__4 = l + l * x_dim1;
	d__4 = z_abs(&x[l + l * x_dim1]);
	z__3.r = x[i__4].r / d__4, z__3.i = x[i__4].i / d__4;
	z__2.r = d__3 * z__3.r, z__2.i = d__3 * z__3.i;
	nrmxl.r = z__2.r, nrmxl.i = z__2.i;
      }
    i__2 = n - l + 1;
    z_div(&z__1, &c_b28, &nrmxl);
    blas_zscal(i__2, &z__1, &x[l + l * x_dim1], 1);
    i__2 = l + l * x_dim1;
    i__3 = l + l * x_dim1;
    z__1.r = x[i__3].r + 1., z__1.i = x[i__3].i + 0.;
    x[i__2].r = z__1.r, x[i__2].i = z__1.i;

    /*              apply the transformation to the remaining columns, */
    /*              updating the norms. */

    lp1 = l + 1;
    if (p < lp1) {
      goto L170;
    }
    i__2 = p;
    for (j = lp1; j <= i__2; ++j) {
      i__3 = n - l + 1;
      blas_zdotc(&z__3, i__3, &x[l + l * x_dim1], 1, &x[l + j * x_dim1], 1);
      z__2.r = -z__3.r, z__2.i = -z__3.i;
      z_div(&z__1, &z__2, &x[l + l * x_dim1]);
      t.r = z__1.r, t.i = z__1.i;
      i__3 = n - l + 1;
      blas_zaxpy(i__3, &t, &x[l + l * x_dim1], 1, &x[l + j * x_dim1], 1);
      if (j < pl || j > pu) {
	goto L150;
      }
      i__3 = j;
      i__4 = j;
      z__1.r = qraux[i__4].r * 0. - qraux[i__4].i * -1., z__1.i = qraux[
									i__4].r * -1. + qraux[i__4].i * 0.;
      if ((d__1 = qraux[i__3].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2))
	  == 0.) {
	goto L150;
      }
      i__3 = j;
      /* Computing 2nd power */
      d__1 = z_abs(&x[l + j * x_dim1]) / qraux[i__3].r;
      tt = 1. - d__1 * d__1;
      tt = max(tt,0.);
      z__1.r = tt, z__1.i = 0.;
      t.r = z__1.r, t.i = z__1.i;
      i__3 = j;
      i__4 = j;
      /* Computing 2nd power */
      d__1 = qraux[i__3].r / work[i__4].r;
      tt = tt * .05 * (d__1 * d__1) + 1.;
      if (tt == 1.) {
	goto L130;
      }
      i__3 = j;
      i__4 = j;
      z_sqrt(&z__2, &t);
      z__1.r = qraux[i__4].r * z__2.r - qraux[i__4].i * z__2.i, z__1.i =
	qraux[i__4].r * z__2.i + qraux[i__4].i * z__2.r;
      qraux[i__3].r = z__1.r, qraux[i__3].i = z__1.i;
      goto L140;
    L130:
      i__3 = j;
      i__4 = n - l;
      d__1 = blas_dznrm2(i__4, &x[l + 1 + j * x_dim1], 1);
      z__1.r = d__1, z__1.i = 0.;
      qraux[i__3].r = z__1.r, qraux[i__3].i = z__1.i;
      i__3 = j;
      i__4 = j;
      work[i__3].r = qraux[i__4].r, work[i__3].i = qraux[i__4].i;
    L140:
    L150:
      /* L160: */
      ;
    }
  L170:

    /*              save the transformation. */

    i__2 = l;
    i__3 = l + l * x_dim1;
    qraux[i__2].r = x[i__3].r, qraux[i__2].i = x[i__3].i;
    i__2 = l + l * x_dim1;
    z__1.r = -nrmxl.r, z__1.i = -nrmxl.i;
    x[i__2].r = z__1.r, x[i__2].i = z__1.i;
  L180:
  L190:
    /* L200: */
    ;
  }
  return;
}

VOID linpack_zqrsl P13C(dcomplex *, x,
			int, ldx,
			int, n,
			int, k,
			dcomplex *, qraux,
			dcomplex *, y,
			dcomplex *, qy,
			dcomplex *, qty,
			dcomplex *, b,
			dcomplex *, rsd,
			dcomplex *, xb,
			int, job, 
			int *, info)
{
  /* System generated locals */
  int x_dim1, x_offset, i__1, i__2, i__3;
  double d__1, d__2;
  dcomplex z__1, z__2, z__3;

  /* Local variables */
  dcomplex temp;
  int cqty;
  int i, j;
  dcomplex t;
  int cb;
  int jj;
  int cr;
  int ju, kp1;
  int cxb, cqy;


  /*     zqrsl applies the output of zqrdc to compute coordinate */
  /*     transformations, projections, and least squares solutions. */
  /*     for k .le. min(n,p), let xk be the matrix */

  /*            xk = (x(jpvt(1)),x(jpvt(2)), ... ,x(jpvt(k))) */

  /*     formed from columnns jpvt(1), ... ,jpvt(k) of the original */
  /*     n x p matrix x that was input to zqrdc (if no pivoting was */
  /*     done, xk consists of the first k columns of x in their */
  /*     original order).  zqrdc produces a factored unitary matrix q */
  /*     and an upper triangular matrix r such that */

  /*              xk = q * (r) */
  /*                       (0) */

  /*     this information is contained in coded form in the arrays */
  /*     x and qraux. */

  /*     on entry */

  /*        x      complex*16(ldx,p). */
  /*               x contains the output of zqrdc. */

  /*        ldx    integer. */
  /*               ldx is the leading dimension of the array x. */

  /*        n      integer. */
  /*               n is the number of rows of the matrix xk.  it must */
  /*               have the same value as n in zqrdc. */

  /*        k      integer. */
  /*               k is the number of columns of the matrix xk.  k */
  /*               must nnot be greater than min(n,p), where p is the */
  /*               same as in the calling sequence to zqrdc. */

  /*        qraux  complex*16(p). */
  /*               qraux contains the auxiliary output from zqrdc. */

  /*        y      complex*16(n) */
  /*               y contains an n-vector that is to be manipulated */
  /*               by zqrsl. */

  /*        job    integer. */
  /*               job specifies what is to be computed.  job has */
  /*               the decimal expansion abcde, with the following */
  /*               meaning. */

  /*                    if a.ne.0, compute qy. */
  /*                    if b,c,d, or e .ne. 0, compute qty. */
  /*                    if c.ne.0, compute b. */
  /*                    if d.ne.0, compute rsd. */
  /*                    if e.ne.0, compute xb. */

  /*               note that a request to compute b, rsd, or xb */
  /*               automatically triggers the computation of qty, for */
  /*               which an array must be provided in the calling */
  /*               sequence. */

  /*     on return */

  /*        qy     complex*16(n). */
  /*               qy conntains q*y, if its computation has been */
  /*               requested. */

  /*        qty    complex*16(n). */
  /*               qty contains ctrans(q)*y, if its computation has */
  /*               been requested.  here ctrans(q) is the conjugate */
  /*               transpose of the matrix q. */

  /*        b      complex*16(k) */
  /*               b contains the solution of the least squares problem */

  /*                    minimize norm2(y - xk*b), */

  /*               if its computation has been requested.  (note that */
  /*               if pivoting was requested in zqrdc, the j-th */
  /*               component of b will be associated with column jpvt(j) */
  /*               of the original matrix x that was input into zqrdc.) */

  /*        rsd    complex*16(n). */
  /*               rsd contains the least squares residual y - xk*b, */
  /*               if its computation has been requested.  rsd is */
  /*               also the orthogonal projection of y onto the */
  /*               orthogonal complement of the column space of xk. */

  /*        xb     complex*16(n). */
  /*               xb contains the least squares approximation xk*b, */
  /*               if its computation has been requested.  xb is also */
  /*               the orthogonal projection of y onto the column space */
  /*               of x. */

  /*        info   integer. */
  /*               info is zero unless the computation of b has */
  /*               been requested and r is exactly singular.  in */
  /*               this case, info is the index of the first zero */
  /*               diagonal element of r and b is left unaltered. */

  /*     the parameters qy, qty, b, rsd, and xb are not referenced */
  /*     if their computation is not requested and in this case */
  /*     can be replaced by dummy variables in the calling program. */
  /*     to save storage, the user may in some cases use the same */
  /*     array for different parameters in the calling sequence.  a */
  /*     frequently occuring example is when one wishes to compute */
  /*     any of b, rsd, or xb and does not need y or qty.  in this */
  /*     case one may identify y, qty, and one of b, rsd, or xb, while */
  /*     providing separate arrays for anything else that is to be */
  /*     computed.  thus the calling sequence */

  /*          call zqrsl(x,ldx,n,k,qraux,y,dum,y,b,y,dum,110,info) */

  /*     will result in the computation of b and rsd, with rsd */
  /*     overwriting y.  more generally, each item in the following */
  /*     list contains groups of permissible identifications for */
  /*     a single callinng sequence. */

  /*          1. (y,qty,b) (rsd) (xb) (qy) */

  /*          2. (y,qty,rsd) (b) (xb) (qy) */

  /*          3. (y,qty,xb) (b) (rsd) (qy) */

  /*          4. (y,qy) (qty,b) (rsd) (xb) */

  /*          5. (y,qy) (qty,rsd) (b) (xb) */

  /*          6. (y,qy) (qty,xb) (b) (rsd) */

  /*     in any group the value returned in the array allocated to */
  /*     the group corresponds to the last member of the group. */

  /*     linpack. this version dated 08/14/78 . */
  /*     g.w. stewart, university of maryland, argonne national lab. */

  /*     zqrsl uses the following functions and subprograms. */

  /*     blas zaxpy,zcopy,zdotc */
  /*     fortran dabs,min0,mod */

  /*     internal variables */



  /*     set info flag. */

  /* Parameter adjustments */
  x_dim1 = ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  --qraux;
  --y;
  --qy;
  --qty;
  --b;
  --rsd;
  --xb;

  /* Function Body */
  *info = 0;

  /*     determine what is to be computed. */

  cqy = job / 10000 != 0;
  cqty = job % 10000 != 0;
  cb = job % 1000 / 100 != 0;
  cr = job % 100 / 10 != 0;
  cxb = job % 10 != 0;
  /* Computing MIN */
  i__1 = k, i__2 = n - 1;
  ju = min(i__1,i__2);

  /*     special action when n=1. */

  if (ju != 0) {
    goto L40;
  }
  if (cqy) {
    qy[1].r = y[1].r, qy[1].i = y[1].i;
  }
  if (cqty) {
    qty[1].r = y[1].r, qty[1].i = y[1].i;
  }
  if (cxb) {
    xb[1].r = y[1].r, xb[1].i = y[1].i;
  }
  if (! cb) {
    goto L30;
  }
  i__1 = x_dim1 + 1;
  i__2 = x_dim1 + 1;
  z__1.r = x[i__2].r * 0. - x[i__2].i * -1., z__1.i = x[i__2].r * -1. + x[
									  i__2].i * 0.;
  if ((d__1 = x[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) != 0.) {
    goto L10;
  }
  *info = 1;
  goto L20;
 L10:
  z_div(&z__1, &y[1], &x[x_dim1 + 1]);
  b[1].r = z__1.r, b[1].i = z__1.i;
 L20:
 L30:
  if (cr) {
    rsd[1].r = 0., rsd[1].i = 0.;
  }
  goto L250;
 L40:

  /*        set up to compute qy or qty. */

  if (cqy) {
    blas_zcopy(n, &y[1], 1, &qy[1], 1);
  }
  if (cqty) {
    blas_zcopy(n, &y[1], 1, &qty[1], 1);
  }
  if (! cqy) {
    goto L70;
  }

  /*           compute qy. */

  i__1 = ju;
  for (jj = 1; jj <= i__1; ++jj) {
    j = ju - jj + 1;
    i__2 = j;
    i__3 = j;
    z__1.r = qraux[i__3].r * 0. - qraux[i__3].i * -1., z__1.i = qraux[
								      i__3].r * -1. + qraux[i__3].i * 0.;
    if ((d__1 = qraux[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 
	0.) {
      goto L50;
    }
    i__2 = j + j * x_dim1;
    temp.r = x[i__2].r, temp.i = x[i__2].i;
    i__2 = j + j * x_dim1;
    i__3 = j;
    x[i__2].r = qraux[i__3].r, x[i__2].i = qraux[i__3].i;
    i__2 = n - j + 1;
    blas_zdotc(&z__3, i__2, &x[j + j * x_dim1], 1, &qy[j], 1);
    z__2.r = -z__3.r, z__2.i = -z__3.i;
    z_div(&z__1, &z__2, &x[j + j * x_dim1]);
    t.r = z__1.r, t.i = z__1.i;
    i__2 = n - j + 1;
    blas_zaxpy(i__2, &t, &x[j + j * x_dim1], 1, &qy[j], 1);
    i__2 = j + j * x_dim1;
    x[i__2].r = temp.r, x[i__2].i = temp.i;
  L50:
    /* L60: */
    ;
  }
 L70:
  if (! cqty) {
    goto L100;
  }

  /*           compute ctrans(q)*y. */

  i__1 = ju;
  for (j = 1; j <= i__1; ++j) {
    i__2 = j;
    i__3 = j;
    z__1.r = qraux[i__3].r * 0. - qraux[i__3].i * -1., z__1.i = qraux[
								      i__3].r * -1. + qraux[i__3].i * 0.;
    if ((d__1 = qraux[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 
	0.) {
      goto L80;
    }
    i__2 = j + j * x_dim1;
    temp.r = x[i__2].r, temp.i = x[i__2].i;
    i__2 = j + j * x_dim1;
    i__3 = j;
    x[i__2].r = qraux[i__3].r, x[i__2].i = qraux[i__3].i;
    i__2 = n - j + 1;
    blas_zdotc(&z__3, i__2, &x[j + j * x_dim1], 1, &qty[j], 1);
    z__2.r = -z__3.r, z__2.i = -z__3.i;
    z_div(&z__1, &z__2, &x[j + j * x_dim1]);
    t.r = z__1.r, t.i = z__1.i;
    i__2 = n - j + 1;
    blas_zaxpy(i__2, &t, &x[j + j * x_dim1], 1, &qty[j], 1);
    i__2 = j + j * x_dim1;
    x[i__2].r = temp.r, x[i__2].i = temp.i;
  L80:
    /* L90: */
    ;
  }
 L100:

  /*        set up to compute b, rsd, or xb. */

  if (cb) {
    blas_zcopy(k, &qty[1], 1, &b[1], 1);
  }
  kp1 = k + 1;
  if (cxb) {
    blas_zcopy(k, &qty[1], 1, &xb[1], 1);
  }
  if (cr && k < n) {
    i__1 = n - k;
    blas_zcopy(i__1, &qty[kp1], 1, &rsd[kp1], 1);
  }
  if (! cxb || kp1 > n) {
    goto L120;
  }
  i__1 = n;
  for (i = kp1; i <= i__1; ++i) {
    i__2 = i;
    xb[i__2].r = 0., xb[i__2].i = 0.;
    /* L110: */
  }
 L120:
  if (! cr) {
    goto L140;
  }
  i__1 = k;
  for (i = 1; i <= i__1; ++i) {
    i__2 = i;
    rsd[i__2].r = 0., rsd[i__2].i = 0.;
    /* L130: */
  }
 L140:
  if (! cb) {
    goto L190;
  }

  /*           compute b. */

  i__1 = k;
  for (jj = 1; jj <= i__1; ++jj) {
    j = k - jj + 1;
    i__2 = j + j * x_dim1;
    i__3 = j + j * x_dim1;
    z__1.r = x[i__3].r * 0. - x[i__3].i * -1., z__1.i = x[i__3].r * -1. + 
      x[i__3].i * 0.;
    if ((d__1 = x[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) != 0.) 
      {
	goto L150;
      }
    *info = j;
    /*           ......exit */
    goto L180;
  L150:
    i__2 = j;
    z_div(&z__1, &b[j], &x[j + j * x_dim1]);
    b[i__2].r = z__1.r, b[i__2].i = z__1.i;
    if (j == 1) {
      goto L160;
    }
    i__2 = j;
    z__1.r = -b[i__2].r, z__1.i = -b[i__2].i;
    t.r = z__1.r, t.i = z__1.i;
    i__2 = j - 1;
    blas_zaxpy(i__2, &t, &x[j * x_dim1 + 1], 1, &b[1], 1);
  L160:
    /* L170: */
    ;
  }
 L180:
 L190:
  if (! cr && ! cxb) {
    goto L240;
  }

  /*           compute rsd or xb as required. */

  i__1 = ju;
  for (jj = 1; jj <= i__1; ++jj) {
    j = ju - jj + 1;
    i__2 = j;
    i__3 = j;
    z__1.r = qraux[i__3].r * 0. - qraux[i__3].i * -1., z__1.i = qraux[
								      i__3].r * -1. + qraux[i__3].i * 0.;
    if ((d__1 = qraux[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 
	0.) {
      goto L220;
    }
    i__2 = j + j * x_dim1;
    temp.r = x[i__2].r, temp.i = x[i__2].i;
    i__2 = j + j * x_dim1;
    i__3 = j;
    x[i__2].r = qraux[i__3].r, x[i__2].i = qraux[i__3].i;
    if (! cr) {
      goto L200;
    }
    i__2 = n - j + 1;
    blas_zdotc(&z__3, i__2, &x[j + j * x_dim1], 1, &rsd[j], 1);
    z__2.r = -z__3.r, z__2.i = -z__3.i;
    z_div(&z__1, &z__2, &x[j + j * x_dim1]);
    t.r = z__1.r, t.i = z__1.i;
    i__2 = n - j + 1;
    blas_zaxpy(i__2, &t, &x[j + j * x_dim1], 1, &rsd[j], 1);
  L200:
    if (! cxb) {
      goto L210;
    }
    i__2 = n - j + 1;
    blas_zdotc(&z__3, i__2, &x[j + j * x_dim1], 1, &xb[j], 1);
    z__2.r = -z__3.r, z__2.i = -z__3.i;
    z_div(&z__1, &z__2, &x[j + j * x_dim1]);
    t.r = z__1.r, t.i = z__1.i;
    i__2 = n - j + 1;
    blas_zaxpy(i__2, &t, &x[j + j * x_dim1], 1, &xb[j], 1);
  L210:
    i__2 = j + j * x_dim1;
    x[i__2].r = temp.r, x[i__2].i = temp.i;
  L220:
    /* L230: */
    ;
  }
 L240:
 L250:
  return;
}
