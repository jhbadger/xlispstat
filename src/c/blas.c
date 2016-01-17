#include "linalg.h"

/****************************************************************************/
/**                                                                        **/
/**                              Utility Funtions                          **/
/**                       Adapted from the f2c distribution                **/
/**                                                                        **/
/****************************************************************************/

LOCAL VOID z_mul P3C(dcomplex *, c, dcomplex *, a, dcomplex *, b)
{
  dcomplex v;
  v.r = a->r * b->r - a->i * b->i;
  v.i = a->r * b->i + a->i * b->r;
  c->r = v.r;
  c->i = v.i;
}

VOID d_cnjg P2C(dcomplex *, r, dcomplex *, z)
{
  r->r = z->r;
  r->i = - z->i;
}

double dcabs1 P1C(dcomplex *, z)
{
  return abs(z->r) + abs(z->i);
}

double d_int P1C(double *, x)
{
  return( (*x>0) ? floor(*x) : -floor(- *x) );
}

double POW2 P1C(double, x) { return x * x; }
double ABS P1C(double, x) { return abs(x); }


/****************************************************************************/
/**                                                                        **/
/**                           Level 1 BLAS routines                        **/
/**                       translated by f2c and modified                   **/
/**                                                                        **/
/****************************************************************************/

double blas_dasum P3C(int, n, double *, dx, int, incx)
{
  /* Local variables */
  int m;
  double dtemp;

  /* takes the sum of the absolute values. */
  /* jack dongarra, linpack, 3/11/78. */
  /* modified 3/93 to return if incx .le. 0. */
  /* modified 12/3/93, array(1) declarations changed to array(*) */

  /* Function Body */
  dtemp = 0.0;
  if (n <= 0)
    return dtemp;
  if (incx != 1) {
    /* code for increment not equal to 1 */
    if (incx < 0) dx -= (n - 1) * incx;
    while (n-- > 0) {
      dtemp += ABS(*dx);
      dx += incx;
    }
  }
  else {
    /* code for increment equal to 1 */

    /* clean-up loop */
    m = n % 6;
    if (m != 0) {
      n -= m;
      while (m-- > 0) {
	dtemp += ABS(*dx);
	dx++;
      }
    }
    for (; n > 0; n -= 6, dx += 6)
      dtemp +=
	ABS(dx[0]) + ABS(dx[1]) + ABS(dx[2]) +
	  ABS(dx[3]) + ABS(dx[4]) + ABS(dx[5]);
  }
  return dtemp;
}

VOID blas_daxpy P6C(int, n,
		    double, da,
		    double *, dx,
		    int, incx,
		    double *, dy,
		    int, incy)
{
  /* Local variables */
  int m;

  /* constant times a vector plus a vector. */
  /* uses unrolled loops for increments equal to one. */
  /* jack dongarra, linpack, 3/11/78. */
  /* modified 12/3/93, array(1) declarations changed to array(*) */

  /* Function Body */
  if (n <= 0 || da == 0.0)
    return;
  if (incx != 1 || incy != 1) {
    /* code for unequal increments or equal increments */
    /* not equal to 1 */
    if (incx < 0) dx -= (n - 1) * incx;
    if (incy < 0) dy -= (n - 1) * incy;
    while (n-- > 0) {
      *dy += da * *dx;
      dx += incx;
      dy += incy;
    }
  }
  else {
    /* code for both increments equal to 1 */

    /* clean-up loop */
    m = n % 4;
    if (m != 0) {
      n -= m;
      while (m-- > 0)
	*dy++ += da * *dx++;
    }
    for (; n > 0; n -= 4, dx += 4, dy += 4) {
      dy[0] += da * dx[0];
      dy[1] += da * dx[1];
      dy[2] += da * dx[2];
      dy[3] += da * dx[3];
    }
  }
}

VOID blas_dcopy P5C(int, n, double *, dx, int, incx, double *, dy, int, incy)
{
  /* Local variables */
  int m;

  /* copies a vector, x, to a vector, y. */
  /* uses unrolled loops for increments equal to one. */
  /* jack dongarra, linpack, 3/11/78. */
  /* modified 12/3/93, array(1) declarations changed to array(*) */

  /* Function Body */
  if (n <= 0)
    return;
  if (incx != 1 || incy != 1) {
    /* code for unequal increments or equal increments not equal to 1 */
    if (incx < 0) dx -= (n - 1) * incx;
    if (incy < 0) dy -= (n - 1) * incy;
    while (n-- > 0) {
      *dy = *dx;
      dx += incx;
      dy += incy;
    }
  }
  else {
    /* code for both increments equal to 1 */

    /* clean-up loop */
    m = n % 7;
    if (m != 0) {
      n -= m;
      while (m-- > 0)
	*dy++ = *dx++;
    }
    for (; n > 0; n -= 7, dx += 7, dy += 7) {
      dy[0] = dx[0];
      dy[1] = dx[1];
      dy[2] = dx[2];
      dy[3] = dx[3];
      dy[4] = dx[4];
      dy[5] = dx[5];
      dy[6] = dx[6];
    }
  }
}

double blas_ddot P5C(int, n, double *, dx, int, incx, double *, dy, int, incy)
{
  /* Local variables */
  int m;
  double dtemp;

  /* forms the dot product of two vectors. */
  /* uses unrolled loops for increments equal to one. */
  /* jack dongarra, linpack, 3/11/78. */
  /* modified 12/3/93, array(1) declarations changed to array(*) */

  /* Function Body */
  dtemp = 0.0;
  if (n <= 0)
    return dtemp;
  if (incx != 1 || incy != 1) {
    /* code for unequal increments or equal increments not equal to 1 */
    if (incx < 0) dx -= (n - 1) * incx;
    if (incy < 0) dy -= (n - 1) * incy;
    while (n-- > 0) {
      dtemp += *dx * *dy;
      dx += incx;
      dy += incy;
    }
  }
  else {
    /* code for both increments equal to 1 */

    /* clean-up loop */
    m = n % 5;
    if (m != 0) {
      n -= m;
      while (m-- > 0)
	dtemp += *dx++ * *dy++;
    }
    for (; n > 0; n -= 5, dx += 5, dy += 5)
      dtemp +=
	dx[0] * dy[0] + dx[1] * dy[1] + dx[2] * dy[2] +
	  dx[3] * dy[3] + dx[4] * dy[4];
  }
  return dtemp;
}

double blas_dnrm2 P3C(int, n, double *, x, int, incx)
{
  /* Local variables */
  double norm, scale, absxi;
  double ssq;

  /*  DNRM2 returns the euclidean norm of a vector via the function */
  /*  name, so that */

  /*     DNRM2 := sqrt( x'*x ) */

  /*  -- This version written on 25-October-1982. */
  /*     Modified on 14-October-1993 to inline the call to DLASSQ. */
  /*     Sven Hammarling, Nag Ltd. */

  /* Function Body */
  if (n < 1)
    norm = 0.0;
  else if (n == 1)
    norm = ABS(*x);
  else {
    if (incx < 0) x -= (n - 1) * incx;
    scale = 0.0;
    ssq = 1.0;

    /* The following loop is equivalent to this call to the LAPACK */
    /* auxiliary routine: */
    /* CALL DLASSQ( N, X, INCX, SCALE, SSQ ) */
    while (n-- > 0) {
      if (*x != 0.0) {
	absxi = ABS(*x);
	if (scale < absxi) {
	  ssq = ssq * POW2(scale / absxi) + 1.0;
	  scale = absxi;
	}
	else
	  ssq += POW2(absxi / scale);
      }
      x += incx;
    }
    norm = scale * sqrt(ssq);
  }

  return norm;
}

VOID blas_drot P7C(int, n,
		   double *, dx,
		   int, incx,
		   double *, dy,
		   int, incy,
		   double, c,
		   double, s)
{
  /* Local variables */
  double dtemp;

  /* applies a plane rotation. */
  /* jack dongarra, linpack, 3/11/78. */
  /* modified 12/3/93, array(1) declarations changed to array(*) */

  /* Function Body */
  if (n <= 0)
    return;
  if (incx != 1 || incy != 1) {
    /* code for unequal increments or equal increments not equal to 1 */
    if (incx < 0) dx -= (n - 1) * incx;
    if (incy < 0) dy -= (n - 1) * incy;
    while (n-- > 0) {
      dtemp = c * *dx + s * *dy;
      *dy   = c * *dy - s * *dx;
      *dx = dtemp;
      dx += incx;
      dy += incy;
    }
  }
  else {
    /* code for both increments equal to 1 */
    for (; n > 0; n--, dx++, dy++) {
      dtemp = c * *dx + s * *dy;
      *dy   = c * *dy - s * *dx;
      *dx = dtemp;
    }
  }
}

VOID blas_drotg P4C(double *, da, double *, db, double *, c, double *, s)
{
  /* Initialized data */
  static double one = 1.0;

  /* Local variables */
  double r, scale, z, roe;

  /* construct givens plane rotation. */
  /* jack dongarra, linpack, 3/11/78. */

  roe = *db;
  if (ABS(*da) > ABS(*db))
    roe = *da;
  scale = ABS(*da) + ABS(*db);
  if (scale == 0.0) {
    *c = 1.0;
    *s = 0.0;
    r = 0.0;
    z = 0.0;
  }
  else {
    double d__1 = POW2(*da / scale);
    double d__2 = POW2(*db / scale);
    r = scale * sqrt(d__1 + d__2);
    r = d_sign(&one, &roe) * r;
    *c = *da / r;
    *s = *db / r;
    z = 1.0;
    if (ABS(*da) > ABS(*db))
      z = *s;
    if (ABS(*db) >= ABS(*da) && *c != 0.0)
      z = 1.0 / *c;
  }
  *da = r;
  *db = z;
}

VOID blas_dscal P4C(int, n, double, da, double *, dx, int, incx)
{
  /* Local variables */
  int m;

  /* scales a vector by a constant. */
  /* uses unrolled loops for increment equal to one. */
  /* jack dongarra, linpack, 3/11/78. */
  /* modified 3/93 to return if incx .le. 0. */
  /* modified 12/3/93, array(1) declarations changed to array(*) */

  /* Function Body */
  if (n <= 0)
    return;
  if (incx != 1) {
    /* code for increment not equal to 1 */
    if (incx < 0) dx -= (n - 1) * incx;
    while (n-- > 0) {
      *dx = da * *dx;
      dx += incx;
    }
  }
  else {
    /* code for increment equal to 1 */

    /* clean-up loop */
    m = n % 5;
    if (m != 0) {
      n -= m;
      while (m-- > 0) {
	*dx = da * *dx;
	dx++;
      }
    }
    for (; n > 0; n -= 5, dx += 5) {
      dx[0] = da * dx[0];
      dx[1] = da * dx[1];
      dx[2] = da * dx[2];
      dx[3] = da * dx[3];
      dx[4] = da * dx[4];
    }
  }
}

VOID blas_dswap P5C(int, n, double *, dx, int, incx, double *, dy, int, incy)
{
  /* Local variables */
  int m;
  double dtemp;

  /* interchanges two vectors. */
  /* uses unrolled loops for increments equal one. */
  /* jack dongarra, linpack, 3/11/78. */
  /* modified 12/3/93, array(1) declarations changed to array(*) */

  /* Function Body */
  if (n <= 0)
    return;
  if (incx != 1 || incy != 1) {
    /* code for unequal increments or equal increments not equal to 1 */
    if (incx < 0) dx -= (n - 1) * incx;
    if (incy < 0) dy -= (n - 1) * incy;
    while (n-- > 0) {
      dtemp = *dx;
      *dx = *dy;
      *dy = dtemp;
      dx += incx;
      dy += incy;
    }
  }
  else {
    /* code for both increments equal to 1 */

    /* clean-up loop */
    m = n % 3;
    if (m != 0) {
      n -= m;
      while (m-- > 0) {
	dtemp = *dx;
	*dx = *dy;
	*dy = dtemp;
	dx++;
	dy++;
      }
    }
    for (; n > 0; n -= 3, dx += 3, dy += 3) {
      dtemp = dx[0];
      dx[0] = dy[0];
      dy[0] = dtemp;
      dtemp = dx[1];
      dx[1] = dy[1];
      dy[1] = dtemp;
      dtemp = dx[2];
      dx[2] = dy[2];
      dy[2] = dtemp;
    }
  }
}

int blas_idamax P3C(int, n, double *, dx, int, incx)
{
  /* System locals */
  int ret_val;

  /* Local variables */
  double dmax;
  int i;

  /* finds the index of element having max. absolute value. */
  /* jack dongarra, linpack, 3/11/78. */
  /* modified 3/93 to return if incx .le. 0. */
  /* modified 12/3/93, array(1) declarations changed to array(*) */

  /* Function Body */
  if (n < 1)
    return -1;
  ret_val = 0;
  if (n == 1)
    return ret_val;
  if (incx != 1) {
    /* code for increment not equal to 1 */
    if (incx < 0) dx -= (n - 1) * incx;
    dmax = ABS(*dx);
    dx += incx;
    for (i = 1; i < n; i++, dx += incx) {
      if (ABS(*dx) > dmax) {
	ret_val = i;
	dmax = ABS(*dx);
      }
    }
  }
  else {
    /* code for increment equal to 1 */
    dmax = ABS(*dx++);
    for (i = 1; i < n; i++, dx++) {
      if (ABS(*dx) > dmax) {
	ret_val = i;
	dmax = ABS(*dx);
      }
    }
  }
  return ret_val;
}

double blas_dzasum P3C(int, n, dcomplex *, zx, int, incx)
{
  /* Local variables */
  static double stemp;


  /* takes the sum of the absolute values. */
  /* jack dongarra, 3/11/78. */
  /* modified 3/93 to return if incx .le. 0. */
  /* modified 12/3/93, array(1) declarations changed to array(*) */

  /* Function Body */
  stemp = 0.0;
  if (n <= 0)
    return stemp;
  if (incx != 1) {
    /* code for increment not equal to 1 */
    if (incx < 0) zx -= (n - 1) * incx;
    while (n-- > 0) {
      stemp += dcabs1(zx);
      zx += incx;
    }
  }
  else {
    /* code for increment equal to 1 */
    while (n-- > 0)
      stemp += dcabs1(zx++);
  }
  return stemp;
}

double blas_dznrm2 P3C(int, n, dcomplex *, x, int, incx)
{
  /* Local variables */
  double temp, norm, scale;
  double ssq;

  /*  DZNRM2 returns the euclidean norm of a vector via the function */
  /*  name, so that */

  /*     DZNRM2 := sqrt( conjg( x' )*x ) */

  /*  -- This version written on 25-October-1982. */
  /*     Modified on 14-October-1993 to inline the call to ZLASSQ. */
  /*     Sven Hammarling, Nag Ltd. */

  /* Function Body */
  if (n < 1)
    norm = 0.0;
  else {
    if (incx < 0) x -= (n - 1) * incx;
    scale = 0.;
    ssq = 1.;

    /* The following loop is equivalent to this call to the LAPACK */
    /* auxiliary routine: */
    /* CALL ZLASSQ( N, X, INCX, SCALE, SSQ ) */
    while (n-- > 0) {
      if (x->r != 0.0) {
	temp = ABS(x->r);
	if (scale < temp) {
	  ssq = ssq * POW2(scale / temp) + 1.0;
	  scale = temp;
	}
	else
	  ssq += POW2(temp / scale);
      }
      if (x->i != 0.0) {
	temp = ABS(x->i);
	if (scale < temp) {
	  ssq = ssq * POW2(scale / temp) + 1.0;
	  scale = temp;
	}
	else
	  ssq += POW2(temp / scale);
      }
      x += incx;
    }
    norm = scale * sqrt(ssq);
  }

  return norm;
}

int blas_izamax P3C(int, n, dcomplex *, zx, int, incx)
{
  /* System generated locals */
  int ret_val;

  /* Local variables */
  double smax;
  int i;
  
  /* finds the index of element having max. absolute value. */
  /* jack dongarra, 1/15/85. */
  /* modified 3/93 to return if incx .le. 0. */
  /* modified 12/3/93, array(1) declarations changed to array(*) */

  /* Function Body */
  if (n < 1)
    return -1;
  ret_val = 0;
  if (n == 1)
    return ret_val;
  if (incx != 1) {
    /* code for increment not equal to 1 */
    if (incx < 0) zx -= (n - 1) * incx;
    smax = dcabs1(zx);
    zx += incx;
    for (i = 1; i < n; i++, zx += incx) {
      if (dcabs1(zx) > smax) {
	ret_val = i;
	smax = dcabs1(zx);
      }
    }
  }
  else {
    /* code for increment equal to 1 */
    smax = dcabs1(zx);
    zx++;
    for (i = 1; i < n; i++, zx++) {
      if (dcabs1(zx) > smax) {
	ret_val = i;
	smax = dcabs1(zx);
      }
    }
  }
  return ret_val;
}

VOID blas_zaxpy P6C(int, n,
		    dcomplex *, za,
		    dcomplex *, zx,
		    int, incx,
		    dcomplex *, zy,
		    int, incy)
{
  /* System locals */
  dcomplex z__1;

  /* constant times a vector plus a vector. */
  /* jack dongarra, 3/11/78. */
  /* modified 12/3/93, array(1) declarations changed to array(*) */

  /* Function Body */
  if (n <= 0)
    return;
  if (dcabs1(za) == 0.0)
    return;
  if (incx != 1 || incy != 1) {
    /* code for unequal increments or equal increments not equal to 1 */
    if (incx < 0) zx -= (n - 1) * incx;
    if (incy < 0) zy -= (n - 1) * incy;
    while (n-- > 0) {
      z__1.r = za->r * zx->r - za->i * zx->i;
      z__1.i = za->r * zx->i + za->i * zx->r;
      zy->r = zy->r + z__1.r;
      zy->i = zy->i + z__1.i;
      zx += incx;
      zy += incy;
    }
  }
  else {
    /* code for both increments equal to 1 */
    for (; n > 0; n--, zx++, zy++) {
      z__1.r = za->r * zx->r - za->i * zx->i;
      z__1.i = za->r * zx->i + za->i * zx->r;
      zy->r = zy->r + z__1.r;
      zy->i = zy->i + z__1.i;
    }
  }
}

VOID blas_zcopy P5C(int, n,
		    dcomplex *, zx,
		    int, incx,
		    dcomplex *, zy,
		    int, incy)
{
  /* copies a vector, x, to a vector, y. */
  /* jack dongarra, linpack, 4/11/78. */
  /* modified 12/3/93, array(1) declarations changed to array(*) */

  /* Function Body */
  if (n <= 0)
    return;
  if (incx != 1 || incy != 1) {
    /* code for unequal increments or equal increments not equal to 1 */
    if (incx < 0) zx -= (n - 1) * incx;
    if (incy < 0) zy -= (n - 1) * incy;
    while (n-- > 0) {
      zy->r = zx->r;
      zy->i = zx->i;
      zx += incx;
      zy += incy;
    }
  }
  else {
    /* code for both increments equal to 1 */
    while (n-- > 0) {
      zy->r = zx->r;
      zy->i = zx->i;
      zx++;
      zy++;
    }
  }
}

VOID blas_zdotc P6C(dcomplex *, ret_val,
		    int, n,
		    dcomplex *, zx,
		    int, incx,
		    dcomplex *, zy,
		    int, incy)
{
  /* System generated locals */
  dcomplex z__2, z__3;

  /* Local variables */
  dcomplex ztemp;

  /* forms the dot product of a vector. */
  /* jack dongarra, 3/11/78. */

  /* Function Body */
  ztemp.r = 0., ztemp.i = 0.;
  ret_val->r = 0.,  ret_val->i = 0.;
  if (n <= 0)
    return;
  if (incx != 1 || incy != 1) {
    /* code for unequal increments or equal increments */
    /* not equal to 1 */
    if (incx < 0) zx -= (n - 1) * incx;
    if (incy < 0) zy -= (n - 1) * incy;
    while (n-- > 0) {
      d_cnjg(&z__3, zx);
      z__2.r = z__3.r * zy->r - z__3.i * zy->i;
      z__2.i = z__3.r * zy->i + z__3.i * zy->r;
      ztemp.r += z__2.r;
      ztemp.i += z__2.i;
      zx += incx;
      zy += incy;
    }
  }
  else {
    /* code for both increments equal to 1 */
    for (; n > 0; n--, zx++, zy++) {
      d_cnjg(&z__3, zx);
      z__2.r = z__3.r * zy->r - z__3.i * zy->i;
      z__2.i = z__3.r * zy->i + z__3.i * zy->r;
      ztemp.r += z__2.r;
      ztemp.i += z__2.i;
    }
  }
  ret_val->r = ztemp.r,  ret_val->i = ztemp.i;
  return ;
}

VOID blas_zdotu P6C(dcomplex *, ret_val,
		    int, n,
		    dcomplex *, zx,
		    int, incx,
		    dcomplex *, zy,
		    int, incy)
{
  /* System generated locals */
  dcomplex z__2;

  /* Local variables */
  static dcomplex ztemp;

  /* forms the dot product of two vectors. */
  /* jack dongarra, 3/11/78. */

  /* Function Body */
  ztemp.r = 0., ztemp.i = 0.;
  ret_val->r = 0.,  ret_val->i = 0.;
  if (n <= 0)
    return;
  if (incx != 1 || incy != 1) {
    /* code for unequal increments or equal increments */
    /* not equal to 1 */
    if (incx < 0) zx -= (n - 1) * incx;
    if (incy < 0) zy -= (n - 1) * incy;
    while (n-- > 0) {
      z__2.r = zx->r * zy->r - zx->i * zy->i;
      z__2.i = zx->r * zy->i + zx->i * zy->r;
      ztemp.r += z__2.r;
      ztemp.i += z__2.i;
      zx += incx;
      zy += incy;
    }
  }
  else {
    /* code for both increments equal to 1 */
    for (; n > 0; n--, zx++, zy++) {
      z__2.r = zx->r * zy->r - zx->i * zy->i;
      z__2.i = zx->r * zy->i + zx->i * zy->r;
      ztemp.r += z__2.r;
      ztemp.i += z__2.i;
    }
  }
  ret_val->r = ztemp.r,  ret_val->i = ztemp.i;
  return ;
}

VOID blas_zdrot P7C(int, n,
		    dcomplex *, zx,
		    int, incx,
		    dcomplex *, zy,
		    int, incy,
		    double, c,
		    double, s)
{
  /* Local variables */
  dcomplex ztemp;

  /* applies a plane rotation, where the cos and sin (c and s) are */
  /* double precision and the vectors zx and zy are double complex. */
  /* jack dongarra, linpack, 3/11/78. */

  /* Function Body */
  if (n <= 0)
    return;
  if (incx != 1 || incy != 1) {
    /* code for unequal increments or equal increments not equal to 1 */
    if (incx < 0) zx -= (n - 1) * incx;
    if (incy < 0) zy -= (n - 1) * incy;
    while (n-- > 0) {
      ztemp.r = c * zx->r + s * zy->r;
      ztemp.i = c * zx->i + s * zy->i;
      zy->r = c * zy->r - s * zx->r;
      zy->i = c * zy->i - s * zx->i;
      zx->r = ztemp.r;
      zx->i = ztemp.i;
      zx += incx;
      zy += incy;
    }
  }
  else {
    /* code for both increments equal to 1 */
    for (; n > 0; n--, zx++, zy++) {
      ztemp.r = c * zx->r + s * zy->r;
      ztemp.i = c * zx->i + s * zy->i;
      zy->r = c * zy->r - s * zx->r;
      zy->i = c * zy->i - s * zx->i;
      zx->r = ztemp.r;
      zx->i = ztemp.i;
    }
  }
}

VOID blas_zdscal P4C(int, n,
		     double, da,
		     dcomplex *, zx,
		     int, incx)
{
  /* scales a vector by a constant. */
  /* jack dongarra, 3/11/78. */
  /* modified to correct problem with negative increment, 8/21/90. */

  /* Function Body */
  if (n <= 0)
    return;
  if (incx != 1) {
    /* code for increment not equal to 1 */
    if (incx < 0) zx -= (n - 1) * incx;
    while (n-- > 0) {
      zx->r = da * zx->r;
      zx->i = da * zx->i;
      zx += incx;
    }
  }
  else {
    /* code for increment equal to 1 */
    for (; n > 0; n--, zx++) {
      zx->r = da * zx->r;
      zx->i = da * zx->i;
    }
  }
}

VOID blas_zrotg P4C(dcomplex *, ca,
		    dcomplex *, cb,
		    double *, c,
		    dcomplex *, s)
{
  /* System generated locals */
  dcomplex z__1, z__2, z__3, z__4;

  /* Local variables */
  double norm;
  dcomplex alpha;
  double scale;

  if (z_abs(ca) == 0.0) {
    *c = 0.0;
    s->r = 1.0;
    s->i = 0.0;
    ca->r = cb->r;
    ca->i = cb->i;
  }
  else {
    double d__1, d__2;
    scale = z_abs(ca) + z_abs(cb);
    z__2.r = scale;
    z__2.i = 0.0;
    z_div(&z__1, ca, &z__2);
    z__4.r = scale;
    z__4.i = 0.0;
    z_div(&z__3, cb, &z__4);
    d__1 = POW2(z_abs(&z__1));
    d__2 = POW2(z_abs(&z__3));
    norm = scale * sqrt(d__1 + d__2);
    d__1 = z_abs(ca);
    z__1.r = ca->r / d__1;
    z__1.i = ca->i / d__1;
    alpha.r = z__1.r;
    alpha.i = z__1.i;
    *c = z_abs(ca) / norm;
    d_cnjg(&z__3, cb);
    z__2.r = alpha.r * z__3.r - alpha.i * z__3.i;
    z__2.i = alpha.r * z__3.i + alpha.i * z__3.r;
    z__1.r = z__2.r / norm;
    z__1.i = z__2.i / norm;
    s->r = z__1.r;
    s->i = z__1.i;
    z__1.r = norm * alpha.r;
    z__1.i = norm * alpha.i;
    ca->r = z__1.r;
    ca->i = z__1.i;
  }
}

VOID blas_zscal P4C(int, n,
		    dcomplex *, za,
		    dcomplex *, zx,
		    int, incx)
{
  /* System generated locals */
  dcomplex z__1;

  /* scales a vector by a constant. */
  /* jack dongarra, 3/11/78. */
  /* modified to correct problem with negative increment, 8/21/90. */

  /* Function Body */
  if (n <= 0)
    return;
  if (incx != 1) {
    /* code for increment not equal to 1 */
    if (incx < 0) zx -= (n - 1) * incx;
    while (n-- > 0) {
      z__1.r = za->r * zx->r - za->i * zx->i;
      z__1.i = za->r * zx->i + za->i * zx->r;
      zx->r = z__1.r;
      zx->i = z__1.i;
      zx += incx;
    }
  }
  else {
    /* code for increment equal to 1 */
    for (; n > 0; n--, zx++) {
      z__1.r = za->r * zx->r - za->i * zx->i;
      z__1.i = za->r * zx->i + za->i * zx->r;
      zx->r = z__1.r;
      zx->i = z__1.i;
    }
  }
}

VOID blas_zswap P5C(int, n,
		    dcomplex *, zx,
		    int, incx,
		    dcomplex *, zy,
		    int, incy)
{
  /* Local variables */
  dcomplex ztemp;
  
  /* interchanges two vectors. */
  /* jack dongarra, 3/11/78. */

  /* Function Body */
  if (n <= 0)
    return;
  if (incx != 1 || incy != 1) {
    /* code for unequal increments or equal increments not equal to 1 */
    if (incx < 0) zx -= (n - 1) * incx;
    if (incy < 0) zy -= (n - 1) * incy;
    while (n-- > 0) {
      ztemp.r = zx->r;
      ztemp.i = zx->i;
      zx->r = zy->r;
      zx->i = zy->i;
      zy->r = ztemp.r;
      zy->i = ztemp.i;
      zx += incx;
      zy += incy;
    }
  }
  else {
    /* code for both increments equal to 1 */
    for (; n > 0; n--, zx++, zy++) {
      ztemp.r = zx->r;
      ztemp.i = zx->i;
      zx->r = zy->r;
      zx->i = zy->i;
      zy->r = ztemp.r;
      zy->i = ztemp.i;
    }
  }
}


/****************************************************************************/
/**                                                                        **/
/**                           Level 2 BLAS routines                        **/
/**                       translated by f2c and modified                   **/
/**                                                                        **/
/****************************************************************************/

LOCAL VOID blas_dzero P3C(int, n, double *, x, int, incx)
{
  if (incx < 0) x -= (n - 1) * incx;
  for (; n > 0; n--, x += incx)
    *x = 0.0;
}

LOCAL VOID blas_zzero P3C(int, n, dcomplex *, z, int, incz)
{
  if (incz < 0) z -= (n - 1) * incz;
  for (; n > 0; n--, z += incz) {
    z->r = 0.0;
    z->i = 0.0;
  }
}

LOCAL VOID blas_zconj P3C(int, n, dcomplex *, z, int, incz)
{
  if (incz < 0) z -= (n - 1) * incz;
  for (; n > 0; n--, z += incz)
    z->i = -z->i;
}
  
VOID blas_dgemv P11C(char *, trans,
		     int, m,
		     int, n,
		     double, alpha,
		     double *, a,
		     int, lda,
		     double *, x,
		     int, incx,
		     double, beta,
		     double *, y,
		     int, incy)
{
  /* Local variables */
  int leny;

  /*
   * DGEMV  performs one of the matrix-vector operations
   *
   *     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
   *
   *  where alpha and beta are scalars, x and y are vectors and A is an
   *  m by n matrix.
   *
   *  Level 2 Blas routine.
   */

  /* Quick return if possible. */
  if (m == 0 || n == 0 || (alpha == 0.0 && beta == 1.0))
    return;

  /* Set LENY, the length of the vector y. */
  leny = (*trans == 'n' || *trans == 'N') ? m : n;

  /* First form  y := beta*y. */
  if (beta != 1.0) {
    if (beta == 0.0)
      blas_dzero(leny, y, incy);
    else
      blas_dscal(leny, beta, y, incy);
  }

  if (alpha == 0.0)
    return;

  if (*trans == 'n' || *trans == 'N') {
    /* Form  y := alpha*A*x + y. */
    if (incx < 0) x -= (n - 1) * incx;
    for (; n > 0; n--, a += lda, x += incx)
      if (*x != 0.0)
	blas_daxpy(m, alpha * *x, a, 1, y, incy);
  }
  else {
    /* Form  y := alpha*A'*x + y. */
    if (incy < 0) y -= (n - 1) * incy;
    for (; n > 0; n--, a += lda, y += incy)
      *y += alpha * blas_ddot(m, a, 1, x, incx);
  }
  return;
}

VOID blas_dtrmv P8C(char *, uplo,
		    char *, trans,
		    char *, diag,
		    int, n,
		    double *, a,
		    int, lda,
		    double *, x,
		    int, incx)
{
  /* Local variables */
  int j;
  int jx, kx;
  int nounit;
  int jx1, jlda;

  /*
   * DTRMV  performs one of the matrix-vector operations
   *
   *     x := A*x,   or   x := A'*x,
   *
   *  where x is an n element vector and  A is an n by n unit, or non-unit,
   *  upper or lower triangular matrix.
   *
   *  Level 2 Blas routine.
   */

  /* Quick return if possible. */

  if (n == 0)
    return;

  nounit = *diag == 'n' || *diag == 'N';

  /* Set up the start point in X if the increment is not unity. This */
  /* will be  ( N - 1 )*INCX  too small for descending loops. */

  kx = (incx <= 0) ? - (n - 1) * incx : 0;

  /* Start the operations. In this version the elements of A are */
  /* accessed sequentially with one pass through A. */

  if (*trans == 'n' || *trans == 'N') {

    /* Form  x := A*x. */

    if (*uplo == 'u' || *uplo == 'U') {
      jx = kx;
      jlda = 0;
      for (j = 0; j < n; j++) {
	if (x[jx] != 0.0) {
	  jx1 = (incx < 0) ? jx - incx : 0;
	  blas_daxpy(j, x[jx], a + jlda, 1, x + jx1, incx);
	  if (nounit)
	    x[jx] *= a[j + jlda];
	}
	jx += incx;
	jlda += lda;
      }
    }
    else {
      kx += (n - 1) * incx;
      jx = kx;
      jlda = (n - 1) * lda;
      for (j = n - 1; j >= 0; j--) {
	if (x[jx] != 0.0) {
	  jx1 = (incx > 0) ? jx + incx : 0;
	  blas_daxpy(n - j - 1, x[jx], a + j + 1 + jlda, -1, x + jx1, -incx);
	  if (nounit)
	    x[jx] *= a[j + jlda];
	}
	jx -= incx;
	jlda -= lda;
      }
    }
  }
  else {

    /* Form  x := A'*x. */

    if (*uplo == 'u' || *uplo == 'U') {
      jx = kx + (n - 1) * incx;
      jlda = (n - 1) * lda;
      for (j = n - 1; j >= 0; j--) {
	if (nounit)
	  x[jx] *= a[j + jlda];
	jx1 = (incx < 0) ? jx - incx : 0;
	x[jx] += blas_ddot(j, a + jlda, -1, x + jx1, -incx);
	jx -= incx;
	jlda -= lda;
      }
    }
    else {
      jx = kx;
      jlda = 0;
      for (j = 0; j < n; j++) {
	if (nounit)
	  x[jx] *= a[j + jlda];
	jx1 = (incx > 0) ? jx + incx : 0;
	x[jx] += blas_ddot(n - j - 1, a + j + 1 + jlda, 1, x + jx1, incx);
	jx += incx;
	jlda += lda;
      }
    }
  }

  return;
}


VOID blas_dger P9C(int,  m,
		   int, n,
		   double, alpha,
		   double *, x,
		   int, incx,
		   double *, y,
		   int, incy,
		   double *, a,
		   int, lda)
{
  /* Local variables */
  int j;

  /*
   * DGER   performs the rank 1 operation
   *
   *     A := alpha*x*y' + A,
   *
   *  where alpha is a scalar, x is an m element vector, y is an n element 
   *  vector and A is an m by n matrix.
   *
   *  Level 2 Blas routine.
   */

  /* Quick return if possible. */
  if (m == 0 || n == 0 || alpha == 0.0)
    return;

  if (incy < 0) y -= (n - 1) * incy;

  /* Start the operations. In this version the elements of A are */
  /* accessed sequentially with one pass through A. */
  for (j = 0; j < n; j++, a += lda, y += incy)
    if (*y != 0.0)
      blas_daxpy(m, alpha * *y, x, incx, a, 1);

  return;
}

VOID blas_dtrsv P8C(char *, uplo,
		    char *, trans,
		    char *, diag,
		    int, n,
		    double *, a,
		    int, lda,
		    double *, x,
		    int, incx)
{
  /* Local variables */
  int j;
  int jx, kx;
  int nounit;
  int jx1, jlda;

  /*
   * DTRSV  solves one of the systems of equations
   *
   *     A*x = b,   or   A'*x = b,
   *
   *  where b and x are n element vectors and A is an n by n unit, or
   *  non-unit, upper or lower triangular matrix.
   *
   *  No test for singularity or near-singularity is included in this
   *  routine. Such tests must be performed before calling this routine.
   *
   *  Level 2 Blas routine.
   */

  /* Quick return if possible. */
  if (n == 0)
    return;

  nounit = *diag == 'n' || *diag == 'N';

  /* Set up the start point in X if the increment is not unity. This */
  /* will be  ( N - 1 )*INCX  too small for descending loops. */

  kx = (incx <= 0) ? - (n - 1) * incx : 0;

  /* Start the operations. In this version the elements of A are */
  /* accessed sequentially with one pass through A. */

  if (*trans == 'n' || *trans == 'N') {
    /* Form  x := inv( A )*x. */
    if (*uplo == 'u' || *uplo == 'U') {
      jx = kx + (n - 1) * incx;
      jlda = (n - 1) * lda;
      for (j = n - 1; j >= 0; j--) {
	if (x[jx] != 0.0) {
	  if (nounit)
	    x[jx] /= a[j + jlda];
	  jx1 = (incx < 0) ? jx - incx : 0;
	  blas_daxpy(j, -x[jx], a + jlda, -1, x + jx1, -incx);
	}
	jx -= incx;
	jlda -= lda;
      }
    }
    else {
      jx = kx;
      jlda = 0;
      for (j = 0; j < n; j++) {
	if (x[jx] != 0.0) {
	  if (nounit)
	    x[jx] /= a[j + jlda];
	  jx1 = (incx > 0) ? jx + incx : 0;
	  blas_daxpy(n - j - 1, -x[jx], a + j + 1 + jlda, 1, x + jx1, incx);
	}
	jx += incx;
	jlda += lda;
      }
    }
  }
  else {

    /* Form  x := inv( A' )*x. */

    if (*uplo == 'u' || *uplo == 'U') {
      jx = kx;
      jlda = 0;
      for (j = 0; j < n; j++) {
	jx1 = (incx < 0) ? jx - incx : 0;
	x[jx] -= blas_ddot(j, a + jlda, 1, x + jx1, incx);
	if (nounit)
	  x[jx] /= a[j + jlda];
	jx += incx;
	jlda += lda;
      }
    }
    else {
      jx = kx + (n - 1) * incx;
      jlda = (n - 1) * lda;
      for (j = n - 1; j >= 0; j--) {
	jx1 = (incx > 0) ? jx + incx : 0;
	x[jx] -= blas_ddot(n - j - 1, a + j + 1 + jlda, -1, x + jx1, -incx);
	if (nounit)
	  x[jx] /= a[j + jlda];
	jx -= incx;
	jlda -= lda;
      }
    }
  }

  return;
}

VOID blas_zgemv P11C(char *, trans,
		     int, m,
		     int, n,
		     dcomplex *, alpha,
		     dcomplex *, a,
		     int, lda,
		     dcomplex *, x,
		     int, incx,
		     dcomplex *, beta,
		     dcomplex *, y,
		     int, incy)
{
  /* Local variables */
  dcomplex temp;
  int leny;
  int noconj;

  /*
   * ZGEMV  performs one of the matrix-vector operations
   *
   *     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,   or
   *
   *     y := alpha*conjg( A' )*x + beta*y,
   *
   *  where alpha and beta are scalars, x and y are vectors and A is an
   *  m by n matrix.
   *
   *  Level 2 Blas routine.
   */

  /* Quick return if possible. */
  if (m == 0 || n == 0 ||
      (alpha->r == 0.0 && alpha->i == 0.0 &&
       (beta->r == 1.0 && beta->i == 0.0)))
    return;

  noconj = *trans == 't' || *trans == 'T';

  /* Set  LENY, the length of the vector y. */
  leny = (*trans == 'n' || *trans == 'N') ? m : n;

  /* Start the operations. In this version the elements of A are */
  /* accessed sequentially with one pass through A. */

  /* First form  y := beta*y. */
  if (beta->r != 1.0 || beta->i != 0.0) {
    if (beta->r == 0.0 && beta->i == 0.0)
      blas_zzero(leny, y, incy);
    else
      blas_zscal(leny, beta, y, incy);
  }

  if (alpha->r == 0.0 && alpha->i == 0.0)
    return;

  if (*trans == 'n' || *trans == 'N') {
    /* Form  y := alpha*A*x + y. */
    if (incx < 0) x -= (n - 1) * incx;
    for (; n > 0; n--, a += lda, x += incx)
      if (x->r != 0.0 || x->i != 0.0) {
	temp.r = alpha->r * x->r - alpha->i * x->i;
	temp.i = alpha->r * x->i + alpha->i * x->r;
	blas_zaxpy(m, &temp, a, 1, y, incy);
      }      
  }
  else {
    /* Form  y := alpha*A'*x + y  or  y := alpha*conjg( A' )*x + y. */
    if (incy < 0) y -= (leny - 1) * incy;
    for (; n > 0; n--, a += lda, y += incy) {
      if (noconj)
	blas_zdotu(&temp, m, a, 1, x, incx);
      else
	blas_zdotc(&temp, m, a, 1, x, incx);
      y->r += alpha->r * temp.r - alpha->i * temp.i;
      y->i += alpha->r * temp.i + alpha->i * temp.r;
    }
  }
  return;
}

VOID blas_ztrmv P8C(char *, uplo,
		    char *, trans,
		    char *, diag,
		    int, n,
		    dcomplex *, a,
		    int, lda,
		    dcomplex *, x,
		    int, incx)
{
  /* System generated locals */
  dcomplex z__1, z__2;

  /* Local variables */
  int j;
  int jx, kx;
  int nounit, noconj;
  int jx1, jlda;

  /*
   * ZTRMV  performs one of the matrix-vector operations
   *
   *     x := A*x,   or   x := A'*x,  or  x := conj( A' )*x
   *
   *  where x is an n element vector and  A is an n by n unit, or non-unit,
   *  upper or lower triangular matrix.
   *
   *  Level 2 Blas routine.
   */

  /* Quick return if possible. */
  if (n == 0)
    return;

  nounit = *diag == 'n' || *diag == 'N';
  noconj = *trans == 't' || *trans == 'T';

  /* Set up the start point in X if the increment is not unity. This */
  /* will be  ( N - 1 )*INCX  too small for descending loops. */

  kx = (incx <= 0) ? - (n - 1) * incx : 0;

  /* Start the operations. In this version the elements of A are */
  /* accessed sequentially with one pass through A. */

  if (*trans == 'n' || *trans == 'N') {

    /* Form  x := A*x. */

    if (*uplo == 'u' || *uplo == 'U') {
      jx = kx;
      jlda = 0;
      for (j = 0; j < n; j++) {
	if (x[jx].r != 0.0 || x[jx].i != 0.0) {
	  jx1 = (incx < 0) ? jx - incx : 0;
	  blas_zaxpy(j, &x[jx], a + jlda, 1, x + jx1, incx);
	  if (nounit) {
	    z__1.r = x[jx].r * a[j + jlda].r - x[jx].i * a[j + jlda].i;
	    z__1.i = x[jx].r * a[j + jlda].i + x[jx].i * a[j + jlda].r;
	    x[jx].r = z__1.r;
	    x[jx].i = z__1.i;
	  }
	}
	jx += incx;
	jlda += lda;
      }
    }
    else {
      kx += (n - 1) * incx;
      jx = kx;
      jlda = (n - 1) * lda;
      for (j = n - 1; j >= 0; --j) {
	if (x[jx].r != 0.0 || x[jx].i != 0.0) {
	  jx1 = (incx > 0) ? jx + incx : 0;
	  blas_zaxpy(n - j - 1, &x[jx], a + j + 1 + jlda, -1, x + jx1, -incx);
	  if (nounit) {
	    z__1.r = x[jx].r * a[j + jlda].r - x[jx].i * a[j + jlda].i;
	    z__1.i = x[jx].r * a[j + jlda].i + x[jx].i * a[j + jlda].r;
	    x[jx].r = z__1.r;
	    x[jx].i = z__1.i;
	  }
	}
	jx -= incx;
	jlda -= lda;
      }
    }
  }
  else {

    /* Form  x := A'*x or x := conj( A' )*x. */

    if (*uplo == 'u' || *uplo == 'U') {
      jx = kx + (n - 1) * incx;
      jlda = (n - 1) * lda;
      for (j = n - 1; j >= 0; j--) {
	if (nounit) {
	  if (noconj) {
	    z__1.r = a[j + jlda].r;
	    z__1.i = a[j + jlda].i;
	  }
	  else
	    d_cnjg(&z__1, a + j + jlda);
	  z__2.r = x[jx].r * z__1.r - x[jx].i * z__1.i;
	  z__2.i = x[jx].r * z__1.i + x[jx].i * z__1.r;
	  x[jx].r = z__2.r;
	  x[jx].i = z__2.i;
	}
	jx1 = (incx < 0) ? jx - incx : 0;
	if (noconj)
	  blas_zdotu(&z__1, j, a + jlda, -1, x + jx1, -incx); 
	else
	  blas_zdotc(&z__1, j, a + jlda, -1, x + jx1, -incx); 
	x[jx].r += z__1.r;
	x[jx].i += z__1.i;
	jx -= incx;
	jlda -= lda;
      }
    }
    else {
      jx = kx;
      jlda = 0;
      for (j = 0; j < n; j++) {
	if (nounit) {
	  if (noconj) {
	    z__1.r = a[j + jlda].r;
	    z__1.i = a[j + jlda].i;
	  }
	  else
	    d_cnjg(&z__1, a + j + jlda);
	  z__2.r = x[jx].r * z__1.r - x[jx].i * z__1.i;
	  z__2.i = x[jx].r * z__1.i + x[jx].i * z__1.r;
	  x[jx].r = z__2.r;
	  x[jx].i = z__2.i;
	}
	jx1 = (incx > 0) ? jx + incx : 0;
	if (noconj)
	  blas_zdotu(&z__1, n - j - 1, a + j + 1 + jlda, 1, x + jx1, incx);
	else
	  blas_zdotc(&z__1, n - j - 1, a + j + 1 + jlda, 1, x + jx1, incx);
	x[jx].r += z__1.r;
	x[jx].i += z__1.i;
	jx += incx;
	jlda += lda;
      }
    }
  }

  return;
}


VOID blas_zgerc P9C(int, m,
		    int, n,
		    dcomplex *, alpha,
		    dcomplex *, x,
		    int, incx,
		    dcomplex *, y,
		    int, incy,
		    dcomplex *, a,
		    int, lda)
{
  /* System generated locals */
  dcomplex z__1;

  /* Local variables */
  dcomplex temp;

  /*
   * ZGERC  performs the rank 1 operation
   *
   *     A := alpha*x*conjg( y' ) + A,
   *
   *  where alpha is a scalar, x is an m element vector, y is an n element 
   *  vector and A is an m by n matrix.
   *
   *  Level 2 Blas routine.
   */

  /* Quick return if possible. */
  if (m == 0 || n == 0 || (alpha->r == 0.0 && alpha->i == 0.0))
    return;

  /* Start the operations. In this version the elements of A are */
  /* accessed sequentially with one pass through A. */

  if (incy < 0) y -= (n - 1) * incy;
  for (; n > 0; n--, a += lda, y += incy)
    if (y->r != 0. || y->i != 0.) {
      d_cnjg(&z__1, y);
      temp.r = alpha->r * z__1.r - alpha->i * z__1.i;
      temp.i = alpha->r * z__1.i + alpha->i * z__1.r;
      blas_zaxpy(m, &temp, x, incx, a, 1);
    }
  return;
}

VOID blas_zgeru P9C(int, m,
		    int, n,
		    dcomplex *, alpha,
		    dcomplex *, x,
		    int, incx,
		    dcomplex *, y,
		    int, incy,
		    dcomplex *, a,
		    int, lda)
{
  /* Local variables */
  dcomplex temp;

  /*
   * ZGERU  performs the rank 1 operation
   *
   *     A := alpha*x*y' + A,
   *
   *  where alpha is a scalar, x is an m element vector, y is an n element
   *  vector and A is an m by n matrix.
   *
   *  Level 2 Blas routine.
   */

  /* Quick return if possible. */
  if (m == 0 || n == 0 || (alpha->r == 0.0 && alpha->i == 0.0))
    return;

  /* Start the operations. In this version the elements of A are */
  /* accessed sequentially with one pass through A. */

  if (incy < 0) y -= (n - 1) * incy;

  for (; n > 0; n--, a += lda, y += incy)
    if (y->r != 0. || y->i != 0.) {
      temp.r = alpha->r * y->r - alpha->i * y->i;
      temp.i = alpha->r * y->i + alpha->i * y->r;
      blas_zaxpy(m, &temp, x, incx, a, 1);
    }
  return;
}

VOID blas_ztrsv P8C(char *, uplo,
		    char *, trans,
		    char *, diag,
		    int, n,
		    dcomplex *, a,
		    int, lda,
		    dcomplex *, x,
		    int, incx)
{
  /* System generated locals */
  dcomplex z__1;

  /* Local variables */
  dcomplex temp;
  int j, jx, kx;
  int noconj, nounit;
  int jx1, jlda;

  /*
   * ZTRSV  solves one of the systems of equations
   *
   *     A*x = b,   or   A'*x = b,   or   conjg( A' )*x = b,
   *
   *  where b and x are n element vectors and A is an n by n unit, or
   *  non-unit, upper or lower triangular matrix.
   *
   *  No test for singularity or near-singularity is included in this
   *  routine. Such tests must be performed before calling this routine.
   *
   *  Level 2 Blas routine.
   */

  /* Quick return if possible. */

  if (n == 0)
    return;

  noconj = *trans == 't' || *trans == 'T';
  nounit = *diag == 'n' || *diag == 'N';

  /* Set up the start point in X if the increment is not unity. This */
  /* will be  ( N - 1 )*INCX  too small for descending loops. */

  kx = (incx <= 0) ? - (n - 1) * incx : 0;
  
  /* Start the operations. In this version the elements of A are */
  /* accessed sequentially with one pass through A. */

  if (*trans == 'n' || *trans == 'N') {

    /* Form  x := inv( A )*x. */

    if (*uplo == 'u' || *uplo == 'U') {
      jx = kx + (n - 1) * incx;
      jlda = (n - 1) * lda;
      for (j = n - 1; j >= 0; --j) {
	if (x[jx].r != 0.0 || x[jx].i != 0.0) {
	  if (nounit) {
	    z_div(&z__1, &x[jx], &a[j + jlda]);
	    x[jx].r = z__1.r;
	    x[jx].i = z__1.i;
	  }
	  temp.r = -x[jx].r;
	  temp.i = -x[jx].i;
	  jx1 = (incx < 0) ? jx - incx : 0;
	  blas_zaxpy(j, &temp, a + jlda, -1, x + jx1, -incx);
	}
	jx -= incx;
	jlda -= lda;
      }
    }
    else {
      jx = kx;
      jlda = 0;
      for (j = 0; j < n; ++j) {
	if (x[jx].r != 0. || x[jx].i != 0.) {
	  if (nounit) {
	    z_div(&z__1, x + jx, a + j + jlda);
	    x[jx].r = z__1.r;
	    x[jx].i = z__1.i;
	  }
	  temp.r = -x[jx].r;
	  temp.i = -x[jx].i;
	  jx1 = (incx < 0) ? 0 : jx + incx;
	  blas_zaxpy(n - j - 1, &temp, a + j + 1 + jlda, 1, x + jx1, incx);
	}
	jx += incx;
	jlda += lda;
      }
    }
  }
  else {

    /* Form  x := inv( A' )*x  or  x := inv( conjg( A' ) )*x. */

    if (*uplo == 'u' || *uplo == 'U') {
      jx = kx;
      jlda = 0;
      for (j = 0; j < n; ++j) {
	temp.r = x[jx].r, temp.i = x[jx].i;
	jx1 = (incx < 0) ? jx - incx : 0;
	if (noconj) {
	  blas_zdotu(&z__1, j, a + jlda, 1, x + jx1, incx);
	  temp.r -= z__1.r;
	  temp.i -= z__1.i;
	  if (nounit) {
	    z_div(&z__1, &temp, a + j + jlda);
	    temp.r = z__1.r;
	    temp.i = z__1.i;
	  }
	}
	else {
	  blas_zdotc(&z__1, j, a + jlda, 1, x + jx1, incx);
	  temp.r -= z__1.r;
	  temp.i -= z__1.i;
	  if (nounit) {
	    d_cnjg(&z__1, &a[j + jlda]);
	    z_div(&z__1, &temp, &z__1);
	    temp.r = z__1.r;
	    temp.i = z__1.i;
	  }
	}
	x[jx].r = temp.r;
	x[jx].i = temp.i;
	jx += incx;
	jlda += lda;
      }
    }
    else {
      jx = kx + (n - 1) * incx;;
      jlda = (n - 1) * lda;
      for (j = n - 1; j >= 0; --j) {
	temp.r = x[jx].r, temp.i = x[jx].i;
	jx1 = (incx < 0) ? 0 : jx + incx;
	if (noconj) {
	  blas_zdotu(&z__1, n - j - 1, a + j + 1 + jlda, -1, x + jx1, -incx);
	  temp.r -= z__1.r;
	  temp.i -= z__1.i;
	  if (nounit) {
	    z_div(&z__1, &temp, a + j + jlda);
	    temp.r = z__1.r;
	    temp.i = z__1.i;
	  }
	}
	else {
	  blas_zdotc(&z__1, n - j - 1, a + j + 1 + jlda, -1, x + jx1, -incx);
	  temp.r -= z__1.r;
	  temp.i -= z__1.i;
	  if (nounit) {
	    d_cnjg(&z__1, &a[j + jlda]);
	    z_div(&z__1, &temp, &z__1);
	    temp.r = z__1.r;
	    temp.i = z__1.i;
	  }
	}
	x[jx].r = temp.r;
	x[jx].i = temp.i;
	jx -= incx;
	jlda -= lda;
      }
    }
  }
  return;
}


/****************************************************************************/
/**                                                                        **/
/**                           Level 3 BLAS routines                        **/
/**                       translated by f2c and modified                   **/
/**                                                                        **/
/****************************************************************************/

VOID blas_dgemm P13C(char *, transa,
		     char *, transb,
		     int, m,
		     int, n,
		     int, k,
		     double, alpha,
		     double *, a,
		     int, lda,
		     double *, b,
		     int, ldb,
		     double, beta,
		     double *, c,
		     int, ldc)
{
  /* Local variables */
  int nota, notb, mm, kk;

  /*
   * DGEMM  performs one of the matrix-matrix operations
   *
   *     C := alpha*op( A )*op( B ) + beta*C,
   *
   *  where  op( X ) is one of
   *
   *     op( X ) = X   or   op( X ) = X',
   *
   *  alpha and beta are scalars, and A, B and C are matrices, with op( A )
   *  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
   *
   *  Level 3 Blas routine.
   */

  /* Set NOTA  as  true if  A is not transposed; compute dimensions. */
  nota = *transa == 'n' || *transa == 'N';
  mm = nota ? m : k;
  kk = nota ? k : m;

  /* Set NOTB  as  true if  B is not transposed. */
  notb = *transb == 'n' || *transb == 'N';

  /* Quick return if possible. */
  if (m == 0 || n == 0 || ((alpha == 0.0 || k == 0) && beta == 1.0))
    return;

  /* And if alpha == zero. */
  if (alpha == 0.0) {
    if (beta == 0.0)
      blas_dzero(m * n, c, 1);
    else if (beta != 1.0)
      blas_dscal(m * n, beta, c, 1);
    return;
  }

  /* Start the operations. */
  if (notb)
    /* Form  C := alpha*op(A)*B + beta*C. */
    for (; n > 0; n--, b += ldb, c += ldc)
      blas_dgemv(transa, mm, kk, alpha, a, lda, b, 1, beta, c, 1);
  else
    /* Form  C := alpha*op(A)*B' + beta*C */
    for (; n > 0; n--, b++, c += ldc)
      blas_dgemv(transa, mm, kk, alpha, a, lda, b, ldb, beta, c, 1);

  return;
}

VOID blas_dtrsm P11C(char *, side,
		     char *, uplo,
		     char *, transa,
		     char *, diag,
		     int, m,
		     int, n,
		     double, alpha,
		     double *, a,
		     int, lda,
		     double *, b,
		     int, ldb)
{
  /* Local variables */
  int lside, i;
  char *trans;

  /*
   * DTRSM  solves one of the matrix equations
   *
   *     op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
   *
   *  where alpha is a scalar, X and B are m by n matrices, A is a unit, or
   *  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of 
   *
   *     op( A ) = A   or   op( A ) = A'.
   *
   *  The matrix X is overwritten on B.
   *
   *  Level 3 Blas routine.
   */

  /* Quick return if possible. */
  if (n == 0)
    return;

  /* And when  alpha == zero. */
  if (alpha == 0.0) {
    blas_dzero(m * n, b, 1);
    return;
  }

  /* Start the operations. */

  lside = *side == 'l' || *side == 'L';

  if (lside) {
    /* Form  B := alpha*inv( op( A ) )*B. */
    for (i = 0; i < n; i++, b += ldb) {
      if (alpha != 1.0)
	blas_dscal(m, alpha, b, 1);
      blas_dtrsv(uplo, transa, diag, m, a, lda, b, 1);
    }
  }
  else {
    trans = (*transa == 'n' || *transa == 'N') ? "T" : "N";
    /* Form  B := alpha*B*inv( op( A ) ). */
    for (i = 0; i < m; i++, b++) {
      if (alpha != 1.0)
	blas_dscal(n, alpha, b, ldb);
      blas_dtrsv(uplo, trans, diag, n, a, lda, b, ldb);
    }
  }
  return;
}

VOID blas_zgemm P13C(char *, transa,
		     char *, transb,
		     int, m,
		     int, n,
		     int, k,
		     dcomplex *, alpha,
		     dcomplex *, a,
		     int, lda,
		     dcomplex *, b,
		     int, ldb,
		     dcomplex *, beta,
		     dcomplex *, c,
		     int, ldc)
{
  /* Local variables */
  int nota, notb, conja, conjb, i, j, l;
  dcomplex temp;

  /*
   * ZGEMM  performs one of the matrix-matrix operations
   *
   *     C := alpha*op( A )*op( B ) + beta*C,
   *
   *  where  op( X ) is one of
   *
   *     op( X ) = X   or   op( X ) = X',
   *
   *  alpha and beta are scalars, and A, B and C are matrices, with op( A )
   *  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
   *
   *  Level 3 Blas routine.
   */

  /* Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not */
  /* conjugated or transposed, and set  CONJA and CONJB  as true if  A  */
  /* and  B  respectively are to be  transposed and conjugated.         */

  nota  = *transa == 'n' || *transa == 'N';
  notb  = *transb == 'n' || *transb == 'N';
  conja = *transa == 'c' || *transa == 'C';
  conjb = *transb == 'c' || *transa == 'C';

  /* Quick return if possible. */
  if (m == 0 || n == 0 ||
      (((alpha->r == 0.0 && alpha->i == 0.0) || k == 0) &&
       (beta->r == 1.0 && beta->i == 0.0)))
    return;

  /* And if alpha == zero (after initializing C). */
  if (beta->r == 0.0 && beta->i == 0.0)
    blas_zzero(m * n, c, 1);
  else if (beta->r != 1.0 || beta->i != 0.0)
    blas_zscal(m * n, beta, c, 1);
  if (alpha->r == 0.0 && alpha->i == 0.0)
    return;

  if (nota) {

    /* Form  C := alpha*A*op( B ) + beta*C. */

    for (j = 0; j < n; j++) {
      for (l = 0; l < k; l++) {
	if (notb)
	  z_mul(&temp, alpha, b + l + j * ldb);
	else if (conjb) {
	  d_cnjg(&temp, b + j + l * ldb);
	  z_mul(&temp, alpha, &temp);
	}
	else
	  z_mul(&temp, alpha, b + j + l *ldb);
	blas_zaxpy(m, &temp, a + l * lda, 1, c + j  * ldc, 1);
      }
    }
  }
  else {

    /* Form  C := alpha*conjg( A' )* op( B ) + beta*C, */
    /* or    C := alpha*A'         * op( B ) + beta*C. */

    for (j = 0; j < n; j++) {
      for (i = 0; i < m; i++) {
	if (conja) {
	  if (notb)
	    blas_zdotc(&temp, k, b + j * ldb, 1, a + i * lda, 1);
	  else if (conjb)
	    blas_zdotu(&temp, k, b + j, ldb, a + i * lda, 1);
	  else
	    blas_zdotc(&temp, k, b + j, ldb, a + i * lda, 1);
	  d_cnjg(&temp, &temp);
	}
	else {
	  if (notb)
	    blas_zdotu(&temp, k, b + j * ldb, 1, a + i * lda, 1);
	  else if (conjb)
	    blas_zdotc(&temp, k, b + j, ldb, a + i * lda, 1);
	  else
	    blas_zdotu(&temp, k, b + j, ldb, a + i * lda, 1);
	}
	z_mul(&temp, alpha, &temp);
	c[i + j * ldc].r += temp.r;
	c[i + j * ldc].i += temp.i;
      }
    }
  }
  return;
}

VOID blas_ztrsm P11C(char *, side,
		     char *, uplo,
		     char *, transa,
		     char *, diag,
		     int, m,
		     int, n,
		     dcomplex *, alpha,
		     dcomplex *, a,
		     int, lda,
		     dcomplex *, b,
		     int, ldb)
{
  /* Local variables */
  int lside, conj, i;
  char *trans;

  /*
   * ZTRSM  solves one of the matrix equations
   *
   *     op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
   *
   *  where alpha is a scalar, X and B are m by n matrices, A is a unit, or
   *  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of 
   *
   *     op( A ) = A   or   op( A ) = A'.
   *
   *  The matrix X is overwritten on B.
   *
   *  Level 3 Blas routine.
   */

  /* Quick return if possible. */
  if (n == 0)
    return;

  /* And when  alpha == zero. */
  if (alpha->r == 0.0 && alpha->i == 0.0) {
    blas_zzero(m * n, b, 1);
    return;
  }

  /* Start the operations. */

  lside = *side == 'l' || *side == 'L';

  if (lside) {
    /* Form  B := alpha*inv( op( A ) )*B. */
    for (i = 0; i < n; i++, b += ldb) {
      if (alpha->r != 1.0 || alpha->i != 0.0)
	blas_zscal(m, alpha, b, 1);
      blas_ztrsv(uplo, transa, diag, m, a, lda, b, 1);
    }
  }
  else {
    trans = (*transa == 'n' || *transa == 'N') ? "T" : "N";
    conj = (*transa == 'c' || *transa == 'C');
    /* Form  B := alpha*B*inv( op( A ) ). */
    for (i = 0; i < m; i++, b++) {
      if (alpha->r != 1.0 || alpha->i != 0.0)
	blas_zscal(n, alpha, b, ldb);
      if (conj)
	blas_zconj(n, b, ldb);
      blas_ztrsv(uplo, trans, diag, n, a, lda, b, ldb);
      if (conj)
	blas_zconj(n, b, ldb);
    }
  }
  return;
}


/****************************************************************************/
/**                                                                        **/
/**                 XLISP-STAT interface to BLAS routines                  **/
/**                                                                        **/
/****************************************************************************/

LOCAL VOID getblasdvec P3C(int, n, double **, pdx, int *, pincr)
{
  LVAL x;
  int size, type, offset, incr;

  x = xlgatvec();
  size = gettvecsize(x);
  type = gettvectype(x);
  offset = getfixnum(xlgafixnum());
  incr = getfixnum(xlgafixnum());

  if (offset < 0 || size < offset + (n - 1) * abs(incr) + 1)
    xlerror("incompatible with access indices", x);

  switch(type) {
  case CD_FLOTYPE:
  case CD_DOUBLE:
    break;
  default:
    xlbadtype(x);
  }

  *pdx = ((double *) gettvecdata(x)) + offset;
  *pincr = incr;
}

LOCAL VOID getblaszvec P3C(int, n, dcomplex **, pzx, int *, pincr)
{
  LVAL x;
  int size, type, offset, incr;

  x = xlgatvec();
  size = gettvecsize(x);
  type = gettvectype(x);
  offset = getfixnum(xlgafixnum());
  incr = getfixnum(xlgafixnum());

  if (offset < 0 || size < offset + (n - 1) * abs(incr) + 1)
    xlerror("incompatible with access indices", x);

  switch(type) {
  case CD_CXFLOTYPE:
  case CD_DCOMPLEX:
    break;
  default:
    xlbadtype(x);
  }

  *pzx = ((dcomplex *) gettvecdata(x)) + offset;
  *pincr = incr;
}

LOCAL VOID getblasdmat P4C(int, m, int, n, double **, pda, int *, plda)
{
  LVAL a, aa;
  int size, type, offset, lda;

  aa = xlgetarg();
  offset = getfixnum(xlgafixnum());
  lda = getfixnum(xlgafixnum());

  a = darrayp(aa) ? getdarraydata(aa) : aa;
  if (! tvecp(a)) xlbadtype(aa);
  size = gettvecsize(a);
  type = gettvectype(a);

  if (n < 0 || m < 0 || lda < 0 || lda < m || offset < 0 ||
      size < offset + lda * (n - 1) + m)
    xlerror("incompatible with access indices", aa);

  switch(type) {
  case CD_FLOTYPE:
  case CD_DOUBLE:
    break;
  default:
    xlbadtype(aa);
  }

  *pda = ((double *) gettvecdata(a)) + offset;
  *plda = lda;
}

LOCAL VOID getblaszmat P4C(int, m, int, n, dcomplex **, pda, int *, plda)
{
  LVAL a, aa;
  int size, type, offset, lda;

  aa = xlgetarg();
  offset = getfixnum(xlgafixnum());
  lda = getfixnum(xlgafixnum());

  a = darrayp(aa) ? getdarraydata(aa) : aa;
  if (! tvecp(a)) xlbadtype(aa);
  size = gettvecsize(a);
  type = gettvectype(a);

  if (n < 0 || m < 0 || lda < 0 || lda < m || offset < 0 ||
      size < offset + lda * (n - 1) + m)
    xlerror("incompatible with access indices", aa);

  switch(type) {
  case CD_CXFLOTYPE:
  case CD_DCOMPLEX:
    break;
  default:
    xlbadtype(aa);
  }

  *pda = ((dcomplex *) gettvecdata(a)) + offset;
  *plda = lda;
}

LOCAL VOID getdcomplex P1C(dcomplex *, z)
{
  LVAL a = xlgetarg();
  switch(ntype(a)) {
  case FIXNUM:
    z->r = (FLOTYPE) getfixnum(a);
    z->i = 0.0;
    break;
  case FLONUM:
    z->r = getflonum(a);
    z->i = 0.0;
    break;
  case COMPLEX:
    z->r = makefloat(getreal(a));
    z->i = makefloat(getimag(a));
    break;
  default:
    xlbadtype(a);
  }
}

LVAL xblasdasum(V)
{
  int n, incx;
  double *dx;

  n = getfixnum(xlgafixnum());
  getblasdvec(n, &dx, &incx);
  xllastarg();

  return cvflonum((FLOTYPE) blas_dasum(n, dx, incx));
}

LVAL xblasdaxpy(V)
{
  int n, incx, incy;
  double a, *dx, *dy;

  n = getfixnum(xlgafixnum());
  a = makefloat(xlgetarg());
  getblasdvec(n, &dx, &incx);
  getblasdvec(n, &dy, &incy);
  xllastarg();

  blas_daxpy(n, a, dx, incx, dy, incy);
  return NIL;
}

LVAL xblasdcopy(V)
{
  int n, incx, incy;
  double *dx, *dy;

  n = getfixnum(xlgafixnum());
  getblasdvec(n, &dx, &incx);
  getblasdvec(n, &dy, &incy);
  xllastarg();

  blas_dcopy(n, dx, incx, dy, incy);
  return NIL;
}

LVAL xblasddot(V)
{
  int n, incx, incy;
  double *dx, *dy;

  n = getfixnum(xlgafixnum());
  getblasdvec(n, &dx, &incx);
  getblasdvec(n, &dy, &incy);
  xllastarg();

  return cvflonum((FLOTYPE) blas_ddot(n, dx, incx, dy, incy));
}

LVAL xblasdnrm2(V)
{
  int n, incx;
  double *dx;

  n = getfixnum(xlgafixnum());
  getblasdvec(n, &dx, &incx);
  xllastarg();

  return cvflonum((FLOTYPE) blas_dnrm2(n, dx, incx));
}

LVAL xblasdrot(V)
{
  int n, incx, incy;
  double *dx, *dy, c, s;

  n = getfixnum(xlgafixnum());
  getblasdvec(n, &dx, &incx);
  getblasdvec(n, &dy, &incy);
  c = makefloat(xlgetarg());
  s = makefloat(xlgetarg());
  xllastarg();

  blas_drot(n, dx, incx, dy, incy, c, s);
  return NIL;
}

LVAL xblasdrotg(V)
{
  double a, b, c, s;

  a = makefloat(xlgetarg());
  b = makefloat(xlgetarg());
  xllastarg();

  blas_drotg(&a, &b, &c, &s);

  xlnumresults = 0;
  xlresults[xlnumresults++] = cvflonum((FLOTYPE) a);
  xlresults[xlnumresults++] = cvflonum((FLOTYPE) b);
  xlresults[xlnumresults++] = cvflonum((FLOTYPE) c);
  xlresults[xlnumresults++] = cvflonum((FLOTYPE) s);
  return xlresults[0];
}

LVAL xblasdscal(V)
{
  int n, incx;
  double a, *dx;

  n = getfixnum(xlgafixnum());
  a = makefloat(xlgetarg());
  getblasdvec(n, &dx, &incx);
  xllastarg();

  blas_dscal(n, a, dx, incx);
  return NIL;
}

LVAL xblasdswap(V)
{
  int n, incx, incy;
  double *dx, *dy;

  n = getfixnum(xlgafixnum());
  getblasdvec(n, &dx, &incx);
  getblasdvec(n, &dy, &incy);
  xllastarg();

  blas_dswap(n, dx, incx, dy, incy);
  return NIL;
}

LVAL xblasidamax(V)
{
  int n, incx;
  double *dx;

  n = getfixnum(xlgafixnum());
  getblasdvec(n, &dx, &incx);
  xllastarg();

  return cvfixnum((FIXTYPE) blas_idamax(n, dx, incx));
}

LVAL xblasdzasum(V)
{
  int n, incx;
  dcomplex *zx;

  n = getfixnum(xlgafixnum());
  getblaszvec(n, &zx, &incx);
  xllastarg();

  return cvflonum((FLOTYPE) blas_dzasum(n, zx, incx));
}

LVAL xblasdznrm2(V)
{
  int n, incx;
  dcomplex *zx;

  n = getfixnum(xlgafixnum());
  getblaszvec(n, &zx, &incx);
  xllastarg();

  return cvflonum((FLOTYPE) blas_dznrm2(n, zx, incx));
}

LVAL xblasizamax(V)
{
  int n, incx;
  dcomplex *zx;

  n = getfixnum(xlgafixnum());
  getblaszvec(n, &zx, &incx);
  xllastarg();

  return cvfixnum((FIXTYPE) blas_izamax(n, zx, incx));
}

LVAL xblaszaxpy(V)
{
  int n, incx, incy;
  dcomplex za, *zx, *zy;

  n = getfixnum(xlgafixnum());
  getdcomplex(&za);
  getblaszvec(n, &zx, &incx);
  getblaszvec(n, &zy, &incy);
  xllastarg();

  blas_zaxpy(n, &za, zx, incx, zy, incy);
  return NIL;
}

LVAL xblaszcopy(V)
{
  int n, incx, incy;
  dcomplex *zx, *zy;

  n = getfixnum(xlgafixnum());
  getblaszvec(n, &zx, &incx);
  getblaszvec(n, &zy, &incy);
  xllastarg();

  blas_zcopy(n, zx, incx, zy, incy);
  return NIL;
}

LVAL xlbaszdotc(V)
{
  int n, incx, incy;
  dcomplex *zx, *zy, val;

  n = getfixnum(xlgafixnum());
  getblaszvec(n, &zx, &incx);
  getblaszvec(n, &zy, &incy);
  xllastarg();

  blas_zdotc(&val, n, zx, incx, zy, incy);
  return newdcomplex(val.r, val.i);
}

LVAL xlbaszdotu(V)
{
  int n, incx, incy;
  dcomplex *zx, *zy, val;

  n = getfixnum(xlgafixnum());
  getblaszvec(n, &zx, &incx);
  getblaszvec(n, &zy, &incy);
  xllastarg();

  blas_zdotu(&val, n, zx, incx, zy, incy);
  return newdcomplex(val.r, val.i);
}

LVAL xlbaszdrot(V)
{
  int n, incx, incy;
  dcomplex *zx, *zy;
  double c, s;

  n = getfixnum(xlgafixnum());
  getblaszvec(n, &zx, &incx);
  getblaszvec(n, &zy, &incy);
  c = makefloat(xlgetarg());
  s = makefloat(xlgetarg());
  xllastarg();

  blas_zdrot(n, zx, incx, zy, incy, c, s);
  return NIL;
}

LVAL xlbaszdscal(V)
{
  int n, incx;
  dcomplex *zx;
  double da;

  n = getfixnum(xlgafixnum());
  da = makefloat(xlgetarg());
  getblaszvec(n, &zx, &incx);
  xllastarg();

  blas_zdscal(n, da, zx, incx);
  return NIL;
}

LVAL xlbaszrotg(V)
{
  dcomplex a, b, s;
  double c;

  getdcomplex(&a);
  getdcomplex(&b);
  xllastarg();

  blas_zrotg(&a, &b, &c, &s);

  xlnumresults = 0;
  xlresults[xlnumresults++] = newdcomplex(a.r, a.i);
  xlresults[xlnumresults++] = newdcomplex(b.r, b.i);
  xlresults[xlnumresults++] = cvflonum((FLOTYPE) c);
  xlresults[xlnumresults++] = newdcomplex(s.r, s.i);
  return xlresults[0];
}

LVAL xlbaszscal(V)
{
  int n, incx;
  dcomplex *zx, da;

  n = getfixnum(xlgafixnum());
  getdcomplex(&da);
  getblaszvec(n, &zx, &incx);
  xllastarg();

  blas_zscal(n, &da, zx, incx);
  return NIL;
}

LVAL xlbaszswap(V)
{
  int n, incx, incy;
  dcomplex *zx, *zy;

  n = getfixnum(xlgafixnum());
  getblaszvec(n, &zx, &incx);
  getblaszvec(n, &zy, &incy);
  xllastarg();

  blas_zswap(n, zx, incx, zy, incy);
  return NIL;
}

LVAL xlblasdgemv(V)
{
  char *trans;
  int m, n, lda, incx, incy;
  double alpha, *a, *x, beta, *y;

  trans = getstring(xlgastring());
  m = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  alpha = makefloat(xlgetarg());
  getblasdmat(m, n, &a, &lda);
  getblasdvec(*trans == 'n' || *trans == 'N' ? n : m, &x, &incx);
  beta = makefloat(xlgetarg());
  getblasdvec(*trans == 'n' || *trans == 'N' ? m : n, &y, &incy);
  xllastarg();
  
  blas_dgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy);
  return NIL;
}

LVAL xlblasdtrmv(V)
{
  char *uplo, *trans, *diag;
  int n, lda, incx;
  double *a, *x;

  uplo = getstring(xlgastring());
  trans = getstring(xlgastring());
  diag = getstring(xlgastring());
  n = getfixnum(xlgafixnum());
  getblasdmat(n, n, &a, &lda);
  getblasdvec(n, &x, &incx);
  xllastarg();

  blas_dtrmv(uplo, trans, diag, n, a, lda, x, incx);
  return NIL;
}

LVAL xlblasdger(V)
{
  int m, n, incx, incy, lda;
  double alpha, *x, *y, *a;

  m = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  alpha = makefloat(xlgetarg());
  getblasdvec(m, &x, &incx);
  getblasdvec(n, &y, &incy);
  getblasdmat(m, n, &a, &lda);
  xllastarg();

  blas_dger(m, n, alpha, x, incx, y, incy, a, lda);
  return NIL;
}

LVAL xlblasdtrsv(V)
{
  char *uplo, *trans, *diag;
  int n, lda, incx, i, ilda;
  double *a, *x;

  uplo = getstring(xlgastring());
  trans = getstring(xlgastring());
  diag = getstring(xlgastring());
  n = getfixnum(xlgafixnum());
  getblasdmat(n, n, &a, &lda);
  getblasdvec(n, &x, &incx);
  xllastarg();

  for (i = 0, ilda = 0; i < n; i++, ilda += lda)
    if (a[ilda + i] == 0.0)
      xlfail("matrix is (numerically) singular");

  blas_dtrsv(uplo, trans, diag, n, a, lda, x, incx);
  return NIL;
}

LVAL xlblaszgemv(V)
{
  char *trans;
  int m, n, lda, incx, incy;
  dcomplex alpha, *a, *x, beta, *y;

  trans = getstring(xlgastring());
  m = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  getdcomplex(&alpha);
  getblaszmat(m, n, &a, &lda);
  getblaszvec(*trans == 'n' || *trans == 'N' ? n : m, &x, &incx);
  getdcomplex(&beta);
  getblaszvec(*trans == 'n' || *trans == 'N' ? m : n, &y, &incy);
  xllastarg();
  
  blas_zgemv(trans, m, n, &alpha, a, lda, x, incx, &beta, y, incy);
  return NIL;
}

LVAL xlblasztrmv(V)
{
  char *uplo, *trans, *diag;
  int n, lda, incx;
  dcomplex *a, *x;

  uplo = getstring(xlgastring());
  trans = getstring(xlgastring());
  diag = getstring(xlgastring());
  n = getfixnum(xlgafixnum());
  getblaszmat(n, n, &a, &lda);
  getblaszvec(n, &x, &incx);
  xllastarg();

  blas_ztrmv(uplo, trans, diag, n, a, lda, x, incx);
  return NIL;
}

LVAL xlblaszgerc(V)
{
  int m, n, incx, incy, lda;
  dcomplex alpha, *x, *y, *a;

  m = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  getdcomplex(&alpha);
  getblaszvec(m, &x, &incx);
  getblaszvec(n, &y, &incy);
  getblaszmat(m, n, &a, &lda);
  xllastarg();

  blas_zgerc(m, n, &alpha, x, incx, y, incy, a, lda);
  return NIL;
}

LVAL xlblaszgeru(V)
{
  int m, n, incx, incy, lda;
  dcomplex alpha, *x, *y, *a;

  m = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  getdcomplex(&alpha);
  getblaszvec(m, &x, &incx);
  getblaszvec(n, &y, &incy);
  getblaszmat(m, n, &a, &lda);
  xllastarg();

  blas_zgeru(m, n, &alpha, x, incx, y, incy, a, lda);
  return NIL;
}

LVAL xlblasztrsv(V)
{
  char *uplo, *trans, *diag;
  int n, lda, incx, i, ilda;
  dcomplex *a, *x;

  uplo = getstring(xlgastring());
  trans = getstring(xlgastring());
  diag = getstring(xlgastring());
  n = getfixnum(xlgafixnum());
  getblaszmat(n, n, &a, &lda);
  getblaszvec(n, &x, &incx);
  xllastarg();

  for (i = 0, ilda = 0; i < n; i++, ilda += lda)
    if (a[ilda + i].r == 0.0 && a[ilda + i].i == 0.0)
      xlfail("matrix is (numerically) singular");

  blas_ztrsv(uplo, trans, diag, n, a, lda, x, incx);
  return NIL;
}

LVAL xlblasdgemm(V)
{
  char *transa, *transb;
  int m, n, k, lda, ldb, ldc;
  double alpha, *a, *b, beta, *c;

  transa = getstring(xlgastring());
  transb = getstring(xlgastring());
  m = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  k = getfixnum(xlgafixnum());
  alpha = makefloat(xlgetarg());
  if (*transa == 'n' || *transa == 'N')
    getblasdmat(m, k, &a, &lda);
  else
    getblasdmat(k, m, &a, &lda);
  if (*transb == 'n' || *transb == 'N')
    getblasdmat(k, n, &b, &ldb);
  else
    getblasdmat(n, k, &b, &ldb);
  beta = makefloat(xlgetarg());
  getblasdmat(m, n, &c, &ldc);
  xllastarg();
  
  blas_dgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc);
  return NIL;
}

LVAL xlblasdtrsm(V)
{
  char *side, *uplo, *transa, *diag;
  int m, n, lda, ldb, i, ilda;
  double alpha, *a, *b;

  side = getstring(xlgastring());
  uplo = getstring(xlgastring());
  transa = getstring(xlgastring());
  diag = getstring(xlgastring());
  m = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  alpha = makefloat(xlgetarg());
  if (*side == 'l'|| *side == 'L')
    getblasdmat(m, m, &a, &lda);
  else
    getblasdmat(n, n, &a, &lda);
  getblasdmat(m, n, &b, &ldb);
  xllastarg();
  
  for (i = 0, ilda = 0; i < n; i++, ilda += lda)
    if (a[ilda + i] == 0.0)
      xlfail("matrix is (numerically) singular");

  blas_dtrsm(side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb);
  return NIL;
}

LVAL xlblaszgemm(V)
{
  char *transa, *transb;
  int m, n, k, lda, ldb, ldc;
  dcomplex alpha, *a, *b, beta, *c;

  transa = getstring(xlgastring());
  transb = getstring(xlgastring());
  m = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  k = getfixnum(xlgafixnum());
  getdcomplex(&alpha);
  if (*transa == 'n' || *transa == 'N')
    getblaszmat(m, k, &a, &lda);
  else
    getblaszmat(k, m, &a, &lda);
  if (*transb == 'n' || *transb == 'N')
    getblaszmat(k, n, &b, &ldb);
  else
    getblaszmat(n, k, &b, &ldb);
  getdcomplex(&beta);
  getblaszmat(m, n, &c, &ldc);
  xllastarg();
  
  blas_zgemm(transa, transb, m, n, k, &alpha, a, lda, b, ldb, &beta, c, ldc);
  return NIL;
}

LVAL xlblasztrsm(V)
{
  char *side, *uplo, *transa, *diag;
  int m, n, lda, ldb, i, ilda;
  dcomplex alpha, *a, *b;

  side = getstring(xlgastring());
  uplo = getstring(xlgastring());
  transa = getstring(xlgastring());
  diag = getstring(xlgastring());
  m = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  getdcomplex(&alpha);
  if (*side == 'l'|| *side == 'L')
    getblaszmat(m, m, &a, &lda);
  else
    getblaszmat(n, n, &a, &lda);
  getblaszmat(m, n, &b, &ldb);
  xllastarg();
  
  for (i = 0, ilda = 0; i < n; i++, ilda += lda)
    if (a[ilda + i].r == 0.0 && a[ilda + i].i == 0.0)
      xlfail("matrix is (numerically) singular");

  blas_ztrsm(side, uplo, transa, diag, m, n, &alpha, a, lda, b, ldb);
  return NIL;
}
