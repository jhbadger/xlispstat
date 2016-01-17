/* EISPACK CH an RS routines, translated by f2c */

#include "linalg.h"

LOCAL double epslon P1C(double, x)
{
  /* System generated locals */
  double d__1;

  /* Local variables */
  double a, b, c, eps;

  /* estimate unit roundoff in quantities of size x. */

  /* this program should function properly on all systems */
  /* satisfying the following two assumptions, */
  /*    1.  the base used in representing floating point */
  /*        numbers is not a power of three. */
  /*    2.  the quantity  a  in statement 10 is represented to */
  /*        the accuracy used in floating point variables */
  /*        that are stored in memory. */
  /* the statement number 10 and the go to 10 are intended to */
  /* force optimizing compilers to generate code satisfying */
  /* assumption 2. */
  /* under these assumptions, it should be true that, */
  /*        a  is not exactly equal to four-thirds, */
  /*        b  has a zero for its last bit or digit, */
  /*        c  is not exactly equal to one, */
  /*        eps  measures the separation of 1.0 from */
  /*             the next larger floating point number. */
  /* the developers of eispack would appreciate being informed */
  /* about any systems where these assumptions do not hold. */

  /* this version dated 4/6/83. */

  a = 1.3333333333333333;
 L10:
  b = a - 1.0;
  c = b + b + b;
  eps = (d__1 = c - 1.0, abs(d__1));
  if (eps == 0.0)
    goto L10;
  return eps * abs(x);
}

LOCAL double pythag P2C(double, a, double, b)
{
  /* System generated locals */
  double d__1, d__2, d__3;

  /* Local variables */
  double p, r, s, t, u;

  /* finds dsqrt(a**2+b**2) without overflow or destructive underflow */

  /* Computing MAX */
  d__1 = abs(a), d__2 = abs(b);
  p = max(d__1,d__2);
  if (p == 0.) {
    goto L20;
  }
  /* Computing MIN */
  d__2 = abs(a), d__3 = abs(b);
  /* Computing 2nd power */
  d__1 = min(d__2,d__3) / p;
  r = d__1 * d__1;
 L10:
  t = r + 4.;
  if (t == 4.) {
    goto L20;
  }
  s = r / t;
  u = s * 2. + 1.;
  p = u * p;
  /* Computing 2nd power */
  d__1 = s / u;
  r = d__1 * d__1 * r;
  goto L10;
 L20:
  return p;
}

LOCAL VOID htribk P8C(int, nm,
		      int, n,
		      double *, ar,
		      double *, ai,
		      double *, tau,
		      int, m,
		      double *, zr,
		      double *, zi)
{
  /* Local variables */
  double h;
  int i, j, k, l;
  double s, si;

  /* this subroutine is a translation of a complex analogue of */
  /* the algol procedure trbak1, num. math. 11, 181-195(1968) */
  /* by martin, reinsch, and wilkinson. */
  /* handbook for auto. comp., vol.ii-linear algebra, 212-226(1971). */

  /* this subroutine forms the eigenvectors of a complex hermitian */
  /* matrix by back transforming those of the corresponding */
  /* real symmetric tridiagonal matrix determined by  htridi. */

  /* on input */

  /*    nm must be set to the row dimension of two-dimensional */
  /*      array parameters as declared in the calling program */
  /*      dimension statement. */

  /*    n is the order of the matrix. */

  /*    ar and ai contain information about the unitary trans- */
  /*      formations used in the reduction by  htridi  in their */
  /*      full lower triangles except for the diagonal of ar. */

  /*    tau contains further information about the transformations. */

  /*    m is the number of eigenvectors to be back transformed. */

  /*    zr contains the eigenvectors to be back transformed */
  /*      in its first m columns. */

  /* on output */

  /*    zr and zi contain the real and imaginary parts, */
  /*      respectively, of the transformed eigenvectors */
  /*      in their first m columns. */

  /* note that the last component of each returned vector */
  /* is real and that vector euclidean norms are preserved. */

  /* questions and comments should be directed to burton s. garbow, */
  /* mathematics and computer science div, argonne national laboratory */

  /* this version dated august 1983. */

  /* ------------------------------------------------------------------ */

  /* Parameter adjustments */
  tau -= 3;
  ai -= nm + 1;
  ar -= nm + 1;
  zi -= nm + 1;
  zr -= nm + 1;

  /* Function Body */
  if (m == 0) {
    goto L200;
  }
  /* .......... transform the eigenvectors of the real symmetric */
  /*            tridiagonal matrix to those of the hermitian */
  /*            tridiagonal matrix. .......... */
  for (k = 1; k <= n; ++k) {

    for (j = 1; j <= m; ++j) {
      zi[k + j * nm] = -zr[k + j * nm] * tau[(k << 1) + 2];
      zr[k + j * nm] *= tau[(k << 1) + 1];
    }
  }

  if (n == 1) {
    goto L200;
  }
  /* .......... recover and apply the householder matrices .......... */
  for (i = 2; i <= n; ++i) {
    l = i - 1;
    h = ai[i + i * nm];
    if (h == 0.) {
      goto L140;
    }

    for (j = 1; j <= m; ++j) {
      s = 0.;
      si = 0.;

      for (k = 1; k <= l; ++k) {
	s = s + ar[i + k * nm] * zr[k + j * nm]
	  - ai[i + k * nm] * zi[k + j * nm];
	si = si + ar[i + k * nm] * zi[k + j * nm]
	  + ai[i + k * nm] * zr[k + j * nm];
      }
      /* .......... double divisions avoid possible underflow .......... */
      s = s / h / h;
      si = si / h / h;

      for (k = 1; k <= l; ++k) {
	zr[k + j * nm] = zr[k + j * nm]
	  - s * ar[i + k * nm] - si * ai[i + k * nm];
	zi[k + j * nm] = zi[k + j * nm]
	  - si * ar[i + k * nm] + s * ai[i + k * nm];
      }

    }

  L140:
    ;
  }

 L200:
  return;
}

LOCAL VOID htridi P8C(int, nm,
		      int, n,
		      double *, ar,
		      double *, ai,
		      double *, d,
		      double *, e,
		      double *, e2,
		      double *, tau)
{
  /* System generated locals */
  double d__1, d__2;

  /* Local variables */
  double f, g, h;
  int i, j, k, l;
  double scale, fi, gi, hh;
  int ii;
  double si;
  int jp1;

  /* this subroutine is a translation of a complex analogue of */
  /* the algol procedure tred1, num. math. 11, 181-195(1968) */
  /* by martin, reinsch, and wilkinson. */
  /* handbook for auto. comp., vol.ii-linear algebra, 212-226(1971). */

  /* this subroutine reduces a complex hermitian matrix */
  /* to a real symmetric tridiagonal matrix using */
  /* unitary similarity transformations. */

  /* on input */

  /*    nm must be set to the row dimension of two-dimensional */
  /*      array parameters as declared in the calling program */
  /*      dimension statement. */

  /*    n is the order of the matrix. */

  /*    ar and ai contain the real and imaginary parts, */
  /*      respectively, of the complex hermitian input matrix. */
  /*      only the lower triangle of the matrix need be supplied. */

  /* on output */

  /*    ar and ai contain information about the unitary trans- */
  /*      formations used in the reduction in their full lower */
  /*      triangles.  their strict upper triangles and the */
  /*      diagonal of ar are unaltered. */

  /*    d contains the diagonal elements of the the tridiagonal matrix. */

  /*    e contains the subdiagonal elements of the tridiagonal */
  /*      matrix in its last n-1 positions.  e(1) is set to zero. */

  /*    e2 contains the squares of the corresponding elements of e. */
  /*      e2 may coincide with e if the squares are not needed. */

  /*    tau contains further information about the transformations. */

  /* calls pythag for  dsqrt(a*a + b*b) . */

  /* questions and comments should be directed to burton s. garbow, */
  /* mathematics and computer science div, argonne national laboratory */

  /* this version dated august 1983. */

  /* ------------------------------------------------------------------ */

  /* Parameter adjustments */
  tau -= 3;
  --e2;
  --e;
  --d;
  ai -= nm + 1;
  ar -= nm + 1;

  /* Function Body */
  tau[(n << 1) + 1] = 1.;
  tau[(n << 1) + 2] = 0.;

  for (i = 1; i <= n; ++i) {
    d[i] = ar[i + i * nm];
  }
  /* .......... for i=n step -1 until 1 do -- .......... */
  for (ii = 1; ii <= n; ++ii) {
    i = n + 1 - ii;
    l = i - 1;
    h = 0.;
    scale = 0.;
    if (l < 1) {
      goto L130;
    }
    /* .......... scale row (algol tol then not needed) .......... */
    for (k = 1; k <= l; ++k) {
      scale = scale + (d__1 = ar[i + k * nm], abs(d__1))
	+ (d__2 = ai[i + k * nm], abs(d__2));
    }

    if (scale != 0.) {
      goto L140;
    }
    tau[(l << 1) + 1] = 1.;
    tau[(l << 1) + 2] = 0.;
  L130:
    e[i] = 0.;
    e2[i] = 0.;
    goto L290;

  L140:
    for (k = 1; k <= l; ++k) {
      ar[i + k * nm] /= scale;
      ai[i + k * nm] /= scale;
      h = h + ar[i + k * nm] * ar[i + k * nm]
	+ ai[i + k * nm] * ai[i + k * nm];
    }

    e2[i] = scale * scale * h;
    g = sqrt(h);
    e[i] = scale * g;
    f = pythag(ar[i + l * nm], ai[i + l * nm]);
    /* .......... form next diagonal element of matrix t .......... */
    if (f == 0.) {
      goto L160;
    }
    tau[(l << 1) + 1] = (ai[i + l * nm] * tau[(i << 1) + 2]
			 - ar[i + l * nm] * tau[(i << 1) + 1]) / f;
    si = (ar[i + l * nm] * tau[(i << 1) + 2] + ai[i + l * nm] * 
	  tau[(i << 1) + 1]) / f;
    h += f * g;
    g = g / f + 1.;
    ar[i + l * nm] = g * ar[i + l * nm];
    ai[i + l * nm] = g * ai[i + l * nm];
    if (l == 1) {
      goto L270;
    }
    goto L170;
  L160:
    tau[(l << 1) + 1] = -tau[(i << 1) + 1];
    si = tau[(i << 1) + 2];
    ar[i + l * nm] = g;
  L170:
    f = 0.;

    for (j = 1; j <= l; ++j) {
      g = 0.;
      gi = 0.;
      /* .......... form element of a*u .......... */
      for (k = 1; k <= j; ++k) {
	g = g + ar[j + k * nm] * ar[i + k * nm]
	  + ai[j + k * nm] * ai[i + k * nm];
	gi = gi - ar[j + k * nm] * ai[i + k * nm]
	  + ai[j + k * nm] * ar[i + k * nm];
      }

      jp1 = j + 1;
      if (l < jp1) {
	goto L220;
      }

      for (k = jp1; k <= l; ++k) {
	g = g + ar[k + j * nm] * ar[i + k * nm]
	  - ai[k + j * nm] * ai[i + k * nm];
	gi = gi - ar[k + j * nm] * ai[i + k * nm]
	  - ai[k + j * nm] * ar[i + k * nm];
      }
      /* .......... form element of p .......... */
    L220:
      e[j] = g / h;
      tau[(j << 1) + 2] = gi / h;
      f = f + e[j] * ar[i + j * nm] - tau[(j << 1) + 2] * ai[i + j * nm];
    }

    hh = f / (h + h);
    /* .......... form reduced a .......... */
    for (j = 1; j <= l; ++j) {
      f = ar[i + j * nm];
      g = e[j] - hh * f;
      e[j] = g;
      fi = -ai[i + j * nm];
      gi = tau[(j << 1) + 2] - hh * fi;
      tau[(j << 1) + 2] = -gi;

      for (k = 1; k <= j; ++k) {
	ar[j + k * nm] = ar[j + k * nm] - f * e[k]
	  - g * ar[i + k * nm] + fi * tau[(k << 1) + 2] + gi * ai[i + k * nm];
	ai[j + k * nm] = ai[j + k * nm] - f * tau[(k << 1) + 2]
	  - g * ai[i + k * nm] - fi * e[k] - gi * ar[i + k * nm];
      }
    }

  L270:
    for (k = 1; k <= l; ++k) {
      ar[i + k * nm] = scale * ar[i + k * nm];
      ai[i + k * nm] = scale * ai[i + k * nm];
    }

    tau[(l << 1) + 2] = -si;
  L290:
    hh = d[i];
    d[i] = ar[i + i * nm];
    ar[i + i * nm] = hh;
    ai[i + i * nm] = scale * sqrt(h);
  }

  return;
}

LOCAL VOID tqlrat P4C(int, n,
		      double *, d,
		      double *, e2,
		      int *, ierr)
{
  /* System generated locals */
  double d__1, d__2;

  /* Local variables */
  double b = 0.0, c = 0.0, f, g, h;
  int i, j, l, m;
  double p, r, s, t;
  int l1, ii;
  int mml;

  /* this subroutine is a translation of the algol procedure tqlrat, */
  /* algorithm 464, comm. acm 16, 689(1973) by reinsch. */

  /* this subroutine finds the eigenvalues of a symmetric */
  /* tridiagonal matrix by the rational ql method. */

  /* on input */

  /*    n is the order of the matrix. */

  /*    d contains the diagonal elements of the input matrix. */

  /*    e2 contains the squares of the subdiagonal elements of the */
  /*      input matrix in its last n-1 positions.  e2(1) is arbitrary. */

  /*  on output */

  /*    d contains the eigenvalues in ascending order.  if an */
  /*      error exit is made, the eigenvalues are correct and */
  /*      ordered for indices 1,2,...ierr-1, but may not be */
  /*      the smallest eigenvalues. */

  /*    e2 has been destroyed. */

  /*    ierr is set to */
  /*      zero       for normal return, */
  /*      j          if the j-th eigenvalue has not been */
  /*                 determined after 30 iterations. */

  /* calls pythag for  dsqrt(a*a + b*b) . */

  /* questions and comments should be directed to burton s. garbow, */
  /* mathematics and computer science div, argonne national laboratory */

  /* this version dated august 1983. */

  /* ------------------------------------------------------------------ */

  /* Parameter adjustments */
  --e2;
  --d;

  /* Function Body */
  *ierr = 0;
  if (n == 1) {
    goto L1001;
  }

  for (i = 2; i <= n; ++i) {
    e2[i - 1] = e2[i];
  }

  f = 0.;
  t = 0.;
  e2[n] = 0.;

  for (l = 1; l <= n; ++l) {
    j = 0;
    h = (d__1 = d[l], abs(d__1)) + sqrt(e2[l]);
    if (t > h) {
      goto L105;
    }
    t = h;
    b = epslon(t);
    c = b * b;
    /* .......... look for small squared sub-diagonal element ........
       .. */
  L105:
    for (m = l; m <= n; ++m) {
      if (e2[m] <= c) {
	goto L120;
      }
      /* .......... e2(n) is always zero, so there is no exit */
      /*            through the bottom of the loop .......... */
    }

  L120:
    if (m == l) {
      goto L210;
    }
  L130:
    if (j == 30) {
      goto L1000;
    }
    ++j;
    /* .......... form shift .......... */
    l1 = l + 1;
    s = sqrt(e2[l]);
    g = d[l];
    p = (d[l1] - g) / (s * 2.);
    r = pythag(p, 1.0);
    d[l] = s / (p + d_sign(&r, &p));
    h = g - d[l];

    for (i = l1; i <= n; ++i) {
      d[i] -= h;
    }

    f += h;
    /* .......... rational ql transformation .......... */
    g = d[m];
    if (g == 0.) {
      g = b;
    }
    h = g;
    s = 0.;
    mml = m - l;
    /* .......... for i=m-1 step -1 until l do -- .......... */
    for (ii = 1; ii <= mml; ++ii) {
      i = m - ii;
      p = g * h;
      r = p + e2[i];
      e2[i + 1] = s * r;
      s = e2[i] / r;
      d[i + 1] = h + s * (h + d[i]);
      g = d[i] - e2[i] / g;
      if (g == 0.) {
	g = b;
      }
      h = g * p / r;
    }

    e2[l] = s * g;
    d[l] = h;
    /* .......... guard against underflow in convergence test ........
       .. */
    if (h == 0.) {
      goto L210;
    }
    if ((d__1 = e2[l], abs(d__1)) <= (d__2 = c / h, abs(d__2))) {
      goto L210;
    }
    e2[l] = h * e2[l];
    if (e2[l] != 0.) {
      goto L130;
    }
  L210:
    p = d[l] + f;
    /* .......... order eigenvalues .......... */
    if (l == 1) {
      goto L250;
    }
    /* .......... for i=l step -1 until 2 do -- .......... */
    for (ii = 2; ii <= l; ++ii) {
      i = l + 2 - ii;
      if (p >= d[i - 1]) {
	goto L270;
      }
      d[i] = d[i - 1];
    }

  L250:
    i = 1;
  L270:
    d[i] = p;
  }

  goto L1001;
  /* .......... set error -- no convergence to an */
  /*            eigenvalue after 30 iterations .......... */
 L1000:
  *ierr = l;
 L1001:
  return;
}

LOCAL VOID tql1 P4C(int, n,
		    double *, d,
		    double *, e,
		    int *, ierr)
{
  /* System generated locals */
  double d__1, d__2;

  /* Local variables */
  double c, f, g, h;
  int i, j, l, m;
  double p, r, s, c2, c3 = 0.0;
  int l1, l2;
  double s2 = 0.0;
  int ii;
  double dl1, el1;
  int mml;
  double tst1, tst2;

  /* this subroutine is a translation of the algol procedure tql1, */
  /* num. math. 11, 293-306(1968) by bowdler, martin, reinsch, and */
  /* wilkinson. */
  /* handbook for auto. comp., vol.ii-linear algebra, 227-240(1971). */

  /* this subroutine finds the eigenvalues of a symmetric */
  /* tridiagonal matrix by the ql method. */

  /* on input */

  /*    n is the order of the matrix. */

  /*    d contains the diagonal elements of the input matrix. */

  /*    e contains the subdiagonal elements of the input matrix */
  /*      in its last n-1 positions.  e(1) is arbitrary. */

  /*  on output */

  /*    d contains the eigenvalues in ascending order.  if an */
  /*      error exit is made, the eigenvalues are correct and */
  /*      ordered for indices 1,2,...ierr-1, but may not be */
  /*      the smallest eigenvalues. */

  /*    e has been destroyed. */

  /*    ierr is set to */
  /*      zero       for normal return, */
  /*      j          if the j-th eigenvalue has not been */
  /*                 determined after 30 iterations. */

  /* calls pythag for  dsqrt(a*a + b*b) . */

  /* questions and comments should be directed to burton s. garbow, */
  /* mathematics and computer science div, argonne national laboratory */

  /* this version dated august 1983. */

  /* ------------------------------------------------------------------ */

  /* Parameter adjustments */
  --e;
  --d;

  /* Function Body */
  *ierr = 0;
  if (n == 1) {
    goto L1001;
  }

  for (i = 2; i <= n; ++i) {
    e[i - 1] = e[i];
  }

  f = 0.;
  tst1 = 0.;
  e[n] = 0.;

  for (l = 1; l <= n; ++l) {
    j = 0;
    h = (d__1 = d[l], abs(d__1)) + (d__2 = e[l], abs(d__2));
    if (tst1 < h) {
      tst1 = h;
    }
    /* .......... look for small sub-diagonal element .......... */
    for (m = l; m <= n; ++m) {
      tst2 = tst1 + (d__1 = e[m], abs(d__1));
      if (tst2 == tst1) {
	goto L120;
      }
      /* .......... e(n) is always zero, so there is no exit */
      /*            through the bottom of the loop .......... */
    }

  L120:
    if (m == l) {
      goto L210;
    }
  L130:
    if (j == 30) {
      goto L1000;
    }
    ++j;
    /* .......... form shift .......... */
    l1 = l + 1;
    l2 = l1 + 1;
    g = d[l];
    p = (d[l1] - g) / (e[l] * 2.);
    r = pythag(p, 1.0);
    d[l] = e[l] / (p + d_sign(&r, &p));
    d[l1] = e[l] * (p + d_sign(&r, &p));
    dl1 = d[l1];
    h = g - d[l];
    if (l2 > n) {
      goto L145;
    }

    for (i = l2; i <= n; ++i) {
      d[i] -= h;
    }

  L145:
    f += h;
    /* .......... ql transformation .......... */
    p = d[m];
    c = 1.;
    c2 = c;
    el1 = e[l1];
    s = 0.;
    mml = m - l;
    /* .......... for i=m-1 step -1 until l do -- .......... */
    for (ii = 1; ii <= mml; ++ii) {
      c3 = c2;
      c2 = c;
      s2 = s;
      i = m - ii;
      g = c * e[i];
      h = c * p;
      r = pythag(p, e[i]);
      e[i + 1] = s * r;
      s = e[i] / r;
      c = p / r;
      p = c * d[i] - s * g;
      d[i + 1] = h + s * (c * g + s * d[i]);
    }

    p = -s * s2 * c3 * el1 * e[l] / dl1;
    e[l] = s * p;
    d[l] = c * p;
    tst2 = tst1 + (d__1 = e[l], abs(d__1));
    if (tst2 > tst1) {
      goto L130;
    }
  L210:
    p = d[l] + f;
    /* .......... order eigenvalues .......... */
    if (l == 1) {
      goto L250;
    }
    /* .......... for i=l step -1 until 2 do -- .......... */
    for (ii = 2; ii <= l; ++ii) {
      i = l + 2 - ii;
      if (p >= d[i - 1]) {
	goto L270;
      }
      d[i] = d[i - 1];
    }

  L250:
    i = 1;
  L270:
    d[i] = p;
  }

  goto L1001;
  /* .......... set error -- no convergence to an */
  /*            eigenvalue after 30 iterations .......... */
 L1000:
  *ierr = l;
 L1001:
  return;
}

LOCAL VOID tql2 P6C(int, nm,
		    int, n,
		    double *, d,
		    double *, e,
		    double *, z,
		    int *, ierr)
{
  /* System generated locals */
  double d__1, d__2;

  /* Local variables */
  double c, f, g, h;
  int i, j, k, l, m;
  double p, r, s, c2, c3 = 0.0;
  int l1, l2;
  double s2 = 0.0;
  int ii;
  double dl1, el1;
  int mml;
  double tst1, tst2;

  /* this subroutine is a translation of the algol procedure tql2, */
  /* num. math. 11, 293-306(1968) by bowdler, martin, reinsch, and */
  /* wilkinson. */
  /* handbook for auto. comp., vol.ii-linear algebra, 227-240(1971). */

  /* this subroutine finds the eigenvalues and eigenvectors */
  /* of a symmetric tridiagonal matrix by the ql method. */
  /* the eigenvectors of a full symmetric matrix can also */
  /* be found if  tred2  has been used to reduce this */
  /* full matrix to tridiagonal form. */

  /* on input */

  /*    nm must be set to the row dimension of two-dimensional */
  /*      array parameters as declared in the calling program */
  /*      dimension statement. */

  /*    n is the order of the matrix. */

  /*    d contains the diagonal elements of the input matrix. */

  /*    e contains the subdiagonal elements of the input matrix */
  /*      in its last n-1 positions.  e(1) is arbitrary. */

  /*    z contains the transformation matrix produced in the */
  /*      reduction by  tred2, if performed.  if the eigenvectors */
  /*      of the tridiagonal matrix are desired, z must contain */
  /*      the identity matrix. */

  /*  on output */

  /*    d contains the eigenvalues in ascending order.  if an */
  /*      error exit is made, the eigenvalues are correct but */
  /*      unordered for indices 1,2,...,ierr-1. */

  /*    e has been destroyed. */

  /*    z contains orthonormal eigenvectors of the symmetric */
  /*      tridiagonal (or full) matrix.  if an error exit is made, */
  /*      z contains the eigenvectors associated with the stored */
  /*      eigenvalues. */

  /*    ierr is set to */
  /*      zero       for normal return, */
  /*      j          if the j-th eigenvalue has not been */
  /*                 determined after 30 iterations. */

  /* calls pythag for  dsqrt(a*a + b*b) . */

  /* questions and comments should be directed to burton s. garbow, */
  /* mathematics and computer science div, argonne national laboratory */

  /* this version dated august 1983. */

  /* ------------------------------------------------------------------ */

  /* Parameter adjustments */
  z -= nm + 1;
  --e;
  --d;

  /* Function Body */
  *ierr = 0;
  if (n == 1) {
    goto L1001;
  }

  for (i = 2; i <= n; ++i) {
    e[i - 1] = e[i];
  }

  f = 0.;
  tst1 = 0.;
  e[n] = 0.;

  for (l = 1; l <= n; ++l) {
    j = 0;
    h = (d__1 = d[l], abs(d__1)) + (d__2 = e[l], abs(d__2));
    if (tst1 < h) {
      tst1 = h;
    }
    /* .......... look for small sub-diagonal element .......... */
    for (m = l; m <= n; ++m) {
      tst2 = tst1 + (d__1 = e[m], abs(d__1));
      if (tst2 == tst1) {
	goto L120;
      }
      /* .......... e(n) is always zero, so there is no exit */
      /*            through the bottom of the loop .......... */
    }

  L120:
    if (m == l) {
      goto L220;
    }
  L130:
    if (j == 30) {
      goto L1000;
    }
    ++j;
    /* .......... form shift .......... */
    l1 = l + 1;
    l2 = l1 + 1;
    g = d[l];
    p = (d[l1] - g) / (e[l] * 2.);
    r = pythag(p, 1.0);
    d[l] = e[l] / (p + d_sign(&r, &p));
    d[l1] = e[l] * (p + d_sign(&r, &p));
    dl1 = d[l1];
    h = g - d[l];
    if (l2 > n) {
      goto L145;
    }

    for (i = l2; i <= n; ++i) {
      d[i] -= h;
    }

  L145:
    f += h;
    /* .......... ql transformation .......... */
    p = d[m];
    c = 1.;
    c2 = c;
    el1 = e[l1];
    s = 0.;
    mml = m - l;
    /* .......... for i=m-1 step -1 until l do -- .......... */
    for (ii = 1; ii <= mml; ++ii) {
      c3 = c2;
      c2 = c;
      s2 = s;
      i = m - ii;
      g = c * e[i];
      h = c * p;
      r = pythag(p, e[i]);
      e[i + 1] = s * r;
      s = e[i] / r;
      c = p / r;
      p = c * d[i] - s * g;
      d[i + 1] = h + s * (c * g + s * d[i]);
      /* .......... form vector .......... */
      for (k = 1; k <= n; ++k) {
	h = z[k + (i + 1) * nm];
	z[k + (i + 1) * nm] = s * z[k + i * nm] + c * h;
	z[k + i * nm] = c * z[k + i * nm] - s * h;
      }

    }

    p = -s * s2 * c3 * el1 * e[l] / dl1;
    e[l] = s * p;
    d[l] = c * p;
    tst2 = tst1 + (d__1 = e[l], abs(d__1));
    if (tst2 > tst1) {
      goto L130;
    }
  L220:
    d[l] += f;
  }
  /* .......... order eigenvalues and eigenvectors .......... */
  for (ii = 2; ii <= n; ++ii) {
    i = ii - 1;
    k = i;
    p = d[i];

    for (j = ii; j <= n; ++j) {
      if (d[j] >= p) {
	goto L260;
      }
      k = j;
      p = d[j];
    L260:
      ;
    }

    if (k == i) {
      goto L300;
    }
    d[k] = d[i];
    d[i] = p;

    for (j = 1; j <= n; ++j) {
      p = z[j + i * nm];
      z[j + i * nm] = z[j + k * nm];
      z[j + k * nm] = p;
    }

  L300:
    ;
  }

  goto L1001;
  /* .......... set error -- no convergence to an */
  /*            eigenvalue after 30 iterations .......... */
 L1000:
  *ierr = l;
 L1001:
  return;
}

LOCAL VOID tred1 P6C(int, nm,
		     int, n,
		     double *, a,
		     double *, d,
		     double *, e,
		     double *, e2)
{
  /* System generated locals */
  double d__1;

  /* Local variables */
  double f, g, h;
  int i, j, k, l;
  double scale;
  int ii, jp1;

  /* this subroutine is a translation of the algol procedure tred1, */
  /* num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson. */
  /* handbook for auto. comp., vol.ii-linear algebra, 212-226(1971). */

  /* this subroutine reduces a real symmetric matrix */
  /* to a symmetric tridiagonal matrix using */
  /* orthogonal similarity transformations. */

  /* on input */

  /*    nm must be set to the row dimension of two-dimensional */
  /*      array parameters as declared in the calling program */
  /*      dimension statement. */

  /*    n is the order of the matrix. */

  /*    a contains the real symmetric input matrix.  only the */
  /*      lower triangle of the matrix need be supplied. */

  /* on output */

  /*    a contains information about the orthogonal trans- */
  /*      formations used in the reduction in its strict lower */
  /*      triangle.  the full upper triangle of a is unaltered. */

  /*    d contains the diagonal elements of the tridiagonal matrix. */

  /*    e contains the subdiagonal elements of the tridiagonal */
  /*      matrix in its last n-1 positions.  e(1) is set to zero. */

  /*    e2 contains the squares of the corresponding elements of e. */
  /*      e2 may coincide with e if the squares are not needed. */

  /* questions and comments should be directed to burton s. garbow, */
  /* mathematics and computer science div, argonne national laboratory */

  /* this version dated august 1983. */

  /* ------------------------------------------------------------------ */

  /* Parameter adjustments */
  --e2;
  --e;
  --d;
  a -= nm + 1;

  /* Function Body */
  for (i = 1; i <= n; ++i) {
    d[i] = a[n + i * nm];
    a[n + i * nm] = a[i + i * nm];
  }
  /* .......... for i=n step -1 until 1 do -- .......... */
  for (ii = 1; ii <= n; ++ii) {
    i = n + 1 - ii;
    l = i - 1;
    h = 0.;
    scale = 0.;
    if (l < 1) {
      goto L130;
    }
    /* .......... scale row (algol tol then not needed) .......... */
    for (k = 1; k <= l; ++k) {
      scale += (d__1 = d[k], abs(d__1));
    }

    if (scale != 0.) {
      goto L140;
    }

    for (j = 1; j <= l; ++j) {
      d[j] = a[l + j * nm];
      a[l + j * nm] = a[i + j * nm];
      a[i + j * nm] = 0.;
    }

  L130:
    e[i] = 0.;
    e2[i] = 0.;
    goto L300;

  L140:
    for (k = 1; k <= l; ++k) {
      d[k] /= scale;
      h += d[k] * d[k];
    }

    e2[i] = scale * scale * h;
    f = d[l];
    d__1 = sqrt(h);
    g = -d_sign(&d__1, &f);
    e[i] = scale * g;
    h -= f * g;
    d[l] = f - g;
    if (l == 1) {
      goto L285;
    }
    /* .......... form a*u .......... */
    for (j = 1; j <= l; ++j) {
      e[j] = 0.;
    }

    for (j = 1; j <= l; ++j) {
      f = d[j];
      g = e[j] + a[j + j * nm] * f;
      jp1 = j + 1;
      if (l < jp1) {
	goto L220;
      }

      for (k = jp1; k <= l; ++k) {
	g += a[k + j * nm] * d[k];
	e[k] += a[k + j * nm] * f;
      }

    L220:
      e[j] = g;
    }
    /* .......... form p .......... */
    f = 0.;

    for (j = 1; j <= l; ++j) {
      e[j] /= h;
      f += e[j] * d[j];
    }

    h = f / (h + h);
    /* .......... form q .......... */
    for (j = 1; j <= l; ++j) {
      e[j] -= h * d[j];
    }
    /* .......... form reduced a .......... */
    for (j = 1; j <= l; ++j) {
      f = d[j];
      g = e[j];

      for (k = j; k <= l; ++k) {
	a[k + j * nm] = a[k + j * nm] - f * e[k] - g * d[k];
      }

    }

  L285:
    for (j = 1; j <= l; ++j) {
      f = d[j];
      d[j] = a[l + j * nm];
      a[l + j * nm] = a[i + j * nm];
      a[i + j * nm] = f * scale;
    }

  L300:
    ;
  }

  return;
}

LOCAL VOID tred2 P6C(int, nm,
		     int, n,
		     double *, a,
		     double *, d,
		     double *, e,
		     double *, z)
{
  /* System generated locals */
  double d__1;

  /* Local variables */
  double f, g, h;
  int i, j, k, l;
  double scale, hh;
  int ii, jp1;

  /* this subroutine is a translation of the algol procedure tred2, */
  /* num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson. */
  /* handbook for auto. comp., vol.ii-linear algebra, 212-226(1971). */

  /* this subroutine reduces a real symmetric matrix to a */
  /* symmetric tridiagonal matrix using and accumulating */
  /* orthogonal similarity transformations. */

  /* on input */

  /*    nm must be set to the row dimension of two-dimensional */
  /*      array parameters as declared in the calling program */
  /*      dimension statement. */

  /*    n is the order of the matrix. */

  /*    a contains the real symmetric input matrix.  only the */
  /*      lower triangle of the matrix need be supplied. */

  /* on output */

  /*    d contains the diagonal elements of the tridiagonal matrix. */

  /*    e contains the subdiagonal elements of the tridiagonal */
  /*      matrix in its last n-1 positions.  e(1) is set to zero. */

  /*    z contains the orthogonal transformation matrix */
  /*      produced in the reduction. */

  /*    a and z may coincide.  if distinct, a is unaltered. */

  /* questions and comments should be directed to burton s. garbow, */
  /* mathematics and computer science div, argonne national laboratory */

  /* this version dated august 1983. */

  /* ------------------------------------------------------------------ */

  /* Parameter adjustments */
  z -= nm + 1;
  --e;
  --d;
  a -= nm + 1;

  /* Function Body */
  for (i = 1; i <= n; ++i) {

    for (j = i; j <= n; ++j) {
      z[j + i * nm] = a[j + i * nm];
    }

    d[i] = a[n + i * nm];
  }

  if (n == 1) {
    goto L510;
  }
  /* .......... for i=n step -1 until 2 do -- .......... */
  for (ii = 2; ii <= n; ++ii) {
    i = n + 2 - ii;
    l = i - 1;
    h = 0.;
    scale = 0.;
    if (l < 2) {
      goto L130;
    }
    /* .......... scale row (algol tol then not needed) .......... */
    for (k = 1; k <= l; ++k) {
      scale += (d__1 = d[k], abs(d__1));
    }

    if (scale != 0.) {
      goto L140;
    }
  L130:
    e[i] = d[l];

    for (j = 1; j <= l; ++j) {
      d[j] = z[l + j * nm];
      z[i + j * nm] = 0.;
      z[j + i * nm] = 0.;
    }

    goto L290;

  L140:
    for (k = 1; k <= l; ++k) {
      d[k] /= scale;
      h += d[k] * d[k];
    }

    f = d[l];
    d__1 = sqrt(h);
    g = -d_sign(&d__1, &f);
    e[i] = scale * g;
    h -= f * g;
    d[l] = f - g;
    /* .......... form a*u .......... */
    for (j = 1; j <= l; ++j) {
      e[j] = 0.;
    }

    for (j = 1; j <= l; ++j) {
      f = d[j];
      z[j + i * nm] = f;
      g = e[j] + z[j + j * nm] * f;
      jp1 = j + 1;
      if (l < jp1) {
	goto L220;
      }

      for (k = jp1; k <= l; ++k) {
	g += z[k + j * nm] * d[k];
	e[k] += z[k + j * nm] * f;
      }

    L220:
      e[j] = g;
    }
    /* .......... form p .......... */
    f = 0.;

    for (j = 1; j <= l; ++j) {
      e[j] /= h;
      f += e[j] * d[j];
    }

    hh = f / (h + h);
    /* .......... form q .......... */
    for (j = 1; j <= l; ++j) {
      e[j] -= hh * d[j];
    }
    /* .......... form reduced a .......... */
    for (j = 1; j <= l; ++j) {
      f = d[j];
      g = e[j];

      for (k = j; k <= l; ++k) {
	z[k + j * nm] = z[k + j * nm] - f * e[k] - g * d[k];
      }

      d[j] = z[l + j * nm];
      z[i + j * nm] = 0.;
    }

  L290:
    d[i] = h;
  }
  /* .......... accumulation of transformation matrices .......... */
  for (i = 2; i <= n; ++i) {
    l = i - 1;
    z[n + l * nm] = z[l + l * nm];
    z[l + l * nm] = 1.;
    h = d[i];
    if (h == 0.) {
      goto L380;
    }

    for (k = 1; k <= l; ++k) {
      d[k] = z[k + i * nm] / h;
    }

    for (j = 1; j <= l; ++j) {
      g = 0.;

      for (k = 1; k <= l; ++k) {
	g += z[k + i * nm] * z[k + j * nm];
      }

      for (k = 1; k <= l; ++k) {
	z[k + j * nm] -= g * d[k];
      }
    }

  L380:
    for (k = 1; k <= l; ++k) {
      z[k + i * nm] = 0.;
    }

  }

 L510:
  for (i = 1; i <= n; ++i) {
    d[i] = z[n + i * nm];
    z[n + i * nm] = 0.;
  }

  z[n + n * nm] = 1.;
  e[1] = 0.;
  return;
}

VOID eispack_ch P12C(int, nm,
		     int, n,
		     double *, ar,
		     double *, ai,
		     double *, w,
		     int, matz,
		     double *, zr,
		     double *, zi,
		     double *, fv1,
		     double *, fv2,
		     double *, fm1,
		     int *, ierr)
{
  /* Local variables */
  int i, j;

  /* this subroutine calls the recommended sequence of */
  /* subroutines from the eigensystem subroutine package (eispack) */
  /* to find the eigenvalues and eigenvectors (if desired) */
  /* of a complex hermitian matrix. */

  /* on input */

  /*    nm  must be set to the row dimension of the two-dimensional */
  /*    array parameters as declared in the calling program */
  /*    dimension statement. */

  /*    n  is the order of the matrix  a=(ar,ai). */

  /*    ar  and  ai  contain the real and imaginary parts, */
  /*    respectively, of the complex hermitian matrix. */

  /*    matz  is an integer variable set equal to zero if */
  /*    only eigenvalues are desired.  otherwise it is set to */
  /*    any non-zero integer for both eigenvalues and eigenvectors. */

  /* on output */

  /*    w  contains the eigenvalues in ascending order. */

  /*    zr  and  zi  contain the real and imaginary parts, */
  /*    respectively, of the eigenvectors if matz is not zero. */

  /*    ierr  is an integer output variable set equal to an error */
  /*       completion code described in the documentation for tqlrat */
  /*       and tql2.  the normal completion code is zero. */

  /*    fv1, fv2, and  fm1  are temporary storage arrays. */

  /* questions and comments should be directed to burton s. garbow, */
  /* mathematics and computer science div, argonne national laboratory */

  /* this version dated august 1983. */

  /* ------------------------------------------------------------------ */

  if (n <= nm) {
    goto L10;
  }
  *ierr = n * 10;
  goto L50;

 L10:
  htridi(nm, n, ar, ai, w, fv1, fv2, fm1);
  if (matz != 0) {
    goto L20;
  }
  /* .......... find eigenvalues only .......... */
  tqlrat(n, w, fv2, ierr);
  goto L50;
  /* .......... find both eigenvalues and eigenvectors .......... */
 L20:
  for (i = 0; i < n; ++i) {
    for (j = 0; j < n; ++j) {
      zr[j + i * nm] = 0.0;
    }
    zr[i + i * nm] = 1.0;
  }

  tql2(nm, n, w, fv1, zr, ierr);
  if (*ierr != 0) {
    goto L50;
  }
  htribk(nm, n, ar, ai, fm1, n, zr, zi);
 L50:
  return;
}

VOID eispack_rs P9C(int, nm,
		    int, n,
		    double *, a,
		    double *, w,
		    int, matz,
		    double *, z,
		    double *, fv1,
		    double *, fv2,
		    int *, ierr)
{
  /* this subroutine calls the recommended sequence of */
  /* subroutines from the eigensystem subroutine package (eispack) */
  /* to find the eigenvalues and eigenvectors (if desired) */
  /* of a real symmetric matrix. */

  /* on input */

  /*    nm  must be set to the row dimension of the two-dimensional */
  /*    array parameters as declared in the calling program */
  /*    dimension statement. */

  /*    n  is the order of the matrix  a. */

  /*    a  contains the real symmetric matrix. */

  /*    matz  is an integer variable set equal to zero if */
  /*    only eigenvalues are desired.  otherwise it is set to */
  /*    any non-zero integer for both eigenvalues and eigenvectors. */

  /* on output */

  /*    w  contains the eigenvalues in ascending order. */

  /*    z  contains the eigenvectors if matz is not zero. */

  /*    ierr  is an integer output variable set equal to an error */
  /*       completion code described in the documentation for tqlrat */
  /*       and tql2.  the normal completion code is zero. */

  /*    fv1  and  fv2  are temporary storage arrays. */

  /* questions and comments should be directed to burton s. garbow, */
  /* mathematics and computer science div, argonne national laboratory */

  /* this version dated august 1983. */

  /* ------------------------------------------------------------------ */

  if (n <= nm) {
    goto L10;
  }
  *ierr = n * 10;
  goto L50;

 L10:
  if (matz != 0) {
    goto L20;
  }
  /* .......... find eigenvalues only .......... */
  tred1(nm, n, a, w, fv1, fv2);
  /*  tqlrat encounters catastrophic underflow on the Vax */
  /* call  tqlrat(n,w,fv2,ierr) */
  tql1(n, w, fv1, ierr);
  goto L50;
  /* .......... find both eigenvalues and eigenvectors .......... */
 L20:
  tred2(nm, n, a, w, fv1, z);
  tql2(nm, n, w, fv1, z, ierr);
 L50:
  return;
}
