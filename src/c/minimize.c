/* minimize - minimization routines for XLISP-STAT                    */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

/*
 * Nonlinear optimization modules adapted from Dennis and Schnabel, 
 * "Numerical Methods for Unconstrained Optimization and Nonlinear
 * Equations."
 */

#include "linalg.h"
                            
/* forward declarations */
static VOID cholsolve P4H(int, double *, double *, double *);
static VOID lsolve P4H(int, double *, double *, double *);
static VOID ltsolve P4H(int, double *, double *, double *);
static VOID modelhess P5H(int, double *, double *, double *, double *);
static double basegradsize P5H(int, double *, double *, double *, double);
static double linesearch_step P6H(double, double, double,
				  double, double, double);


/************************************************************************/
/**                                                                    **/
/**                         Utility Functions                          **/
/**                                                                    **/
/************************************************************************/

static double Max P2C(double, a, double, b)
{
  return(a > b ? a : b);
}

static double Min P2C(double, a, double, b)
{
  return(a > b ? b : a);
}

#define Copy(n, x, y) blas_dcopy(n, x, 1, y, 1)
#define Scale(n, a, x) blas_dscal(n, a, x, 1)
#define Axpy(n, a, x, y) blas_daxpy(n, a, x, 1, y, 1)
#define Dot(n, x, y) blas_ddot(n, x, 1, y, 1)
#define Nrm2(n, x) blas_dnm2(n, x, 1)

static double maxrelsize P4C(int, n, double *, x, double *, y, double *, s)
{
  double value, ax, ay, si, tmp;

  value = 0.0;
  while (n-- > 0) {
    ax = *x++; if (ax < 0.0) ax = -ax; /* ax = fabs(*x++) */
    ay = *y++; if (ay < 0.0) ay = -ay; /* ay = fabs(*y++) */
    si = 1.0 / *s++;
    tmp = ax / (ay > si ? ay : si);    /* tmp = ax / Max(ay, si) */
    if (value < tmp) value = tmp;      /* value = Max(value, tmp) */
  }
  return value;
}


/************************************************************************/
/**                                                                    **/
/**                    Cholesky Solving Functions                      **/
/**                                                                    **/
/************************************************************************/

/* solve (L L^T) s = -g for s */
static VOID cholsolve P4C(int, n, double *, g, double *, L, double *, s)
{
  /* solve Ly = g */
  lsolve(n, g, L, s);
  
  /* solve L^Ts = y */
  ltsolve(n, s, L, s);

  Scale(n, -1.0, s);
}

/* solve Ly = b for y */
static VOID lsolve P4C(int, n, double *, b, double *, L, double *, y)
{
  int i, in;
  double Lii;

  if ((Lii = L[0]) != 0) y[0] = b[0] / Lii;
  for (i = 1, in = n; i < n; i++, in += n) {
    y[i] = b[i] - Dot(i, L + in, y);
    if ((Lii = L[in + i]) != 0) y[i] /= Lii;
  }
}

/* solve (L^T)x = y for x */
static VOID ltsolve P4C(int, n, double *, y, double *, L, double *, x)
{
  int i, in;
  double Lii;

  Copy(n, y, x);
  if ((Lii = L[n * n - 1]) != 0.0) x[n - 1] /= Lii;
  for (i = n - 2, in = i * n; i >= 0; i--, in -= n) {
    Axpy(i + 1, -x[i+1], L + in + n, x);
    if ((Lii = L[in + i]) != 0.0) x[i] /= Lii;
  }    
}

static VOID modelhess P5C(int, n,
			  double *, sx,
			  double *, H,
			  double *, L,
			  double *, hessadd)
{
  int i, j, in, ii, ij;
  double sqrteps, maxdiag, mindiag, maxoff, maxoffl, maxposdiag, mu,
    maxadd, maxev, minev, offrow, sdd;

  /* scale H on both sides with sx */
  for (i = 0, in = 0; i < n; i++, in += n)
    for (j = 0, ij = in; j < n; j++, ij++)
      H[ij] /= sx[i] * sx[j];

  /* 
   * find mu to add to diagonal so no diagonal elements are negative
   * and largest diagonal dominates largest off diagonal element in H 
   */
  sqrteps = sqrt(macheps());
  for (mindiag = maxdiag = H[0], i = 1, in = n; i < n; i++, in += n) {
    maxdiag = Max(maxdiag, H[in + i]);
    mindiag = Min(mindiag, H[in + i]);
  }
  maxposdiag = Max(0.0, maxdiag);
  
  if (mindiag <= sqrteps * maxposdiag) {
    mu = 2 * (maxposdiag - mindiag) * sqrteps - mindiag;
    maxdiag += mu;
  }
  else mu = 0.0;
  
  if (n > 1) {
    for (maxoff = fabs(H[1]), i = 0, in = 0; i < n; i++, in += n) 
      for (j = i + 1, ij = in + j; j < n; j++, ij++) 
        maxoff = Max(maxoff, fabs(H[ij]));
  }
  else maxoff = 0.0;

  if (maxoff * (1 + 2 * sqrteps) > maxdiag) {
    mu += (maxoff - maxdiag) + 2 * sqrteps * maxoff;
    maxdiag = maxoff * (1 + 2 * sqrteps);
  }
  
  if (maxdiag == 0.0) {
    mu = 1;
    maxdiag = 1;
  }

  if (mu > 0)
    for (i = 0, in = 0; i < n; i++, in += n)
      H[in + i] += mu;

  maxoffl = sqrt(Max(maxdiag, maxoff / n));

  /*
   * compute the perturbed Cholesky decomposition
   */
  Copy(n * n, H, L);
  choldecomp(L, n, maxoffl, &maxadd);

  /*
   * add something to diagonal, if needed, to make positive definite
   * and recompute factorization
   */
  if (maxadd > 0) {
    maxev = H[0];
    minev = H[0];
    for (i = 0, in = 0; i < n; i++, in += n) {
      for (offrow = 0.0, j = 0, ij = in; j < n; j++, ij++) 
        if (i != j) offrow += fabs(H[ij]);
      ii = in + i;
      maxev = Max(maxev, H[ii] + offrow);
      minev = Min(minev, H[ii] - offrow);
    }
    sdd = (maxev - minev) * sqrteps - minev;
    sdd = Max(sdd, 0.0);
    mu = Min(maxadd, sdd);
    for (i = 0, in = 0; i < n; i++, in += n)
      H[in + i] += mu;
    Copy(n * n, H, L);
    choldecomp(L, n, maxoffl, &maxadd);
    *hessadd = mu;
  }
  else *hessadd = 0.0;

  /* unscale H and L */
  for (i = 0, in = 0; i < n; i++, in += n)
    for (j = 0, ij = in; j < n; j++, ij++) {
      H[ij] *= sx[i] * sx[j];
      L[ij] *= sx[i];
    }
}

/************************************************************************/
/**                                                                    **/
/**                        Stopping Criteria                           **/
/**                                                                    **/
/************************************************************************/

static double basegradsize P5C(int, n,
			       double *, delf,
			       double *, x,
			       double *, sx,
			       double, scale)
{
  int i;
  double term, size;

  size = 0.0;
  if (scale > 0.0) {
    for (i = 0; i < n; i++) {
      term = fabs(delf[i]) * Max(fabs(x[i]), 1.0 / sx[i]) / scale;
      size = Max(size, term);
    }
  }
  return size;
}


/************************************************************************/
/**                                                                    **/
/**                     Backtracking Line Search                       **/
/**                                                                    **/
/************************************************************************/

static double linesearch_step P6C(double, f,
				  double, initslope,
				  double, lambda,
				  double, newf,
				  double, lambdaprev,
				  double, fprev)
{
  double lambdatemp, a, b, disc, f1, f2, a11, a12, a21, a22, del;

  if (lambda == 1.0) { /* first backtrack, quadratic fit */
    lambdatemp = - initslope / (2 * (newf - f - initslope));
  }
  else { /* all subsequent backtracks, cubic fit */
    del = lambda - lambdaprev;
    f1 = newf - f - lambda * initslope;
    f2 = fprev - f - lambdaprev * initslope;
    a11 = 1.0 / (lambda * lambda);
    a12 = -1.0 / (lambdaprev * lambdaprev);
    a21 = -lambdaprev * a11;
    a22 = -lambda * a12;
    a = (a11 * f1 + a12 * f2) / del;
    b = (a21 * f1 + a22 * f2) / del;
    disc = b * b - 3 * a * initslope;
    if (a == 0) { /* cubic is a quadratic */
      lambdatemp = - initslope / (2 * b);
    }
    else { /* legitimate cubic */
      lambdatemp = (-b + sqrt(disc)) / (3 * a);
    }
    lambdatemp = Min(lambdatemp, 0.5 * lambda);
  }
  return lambdatemp;
}

/**** This error checking needs cleaning up */
static LVAL xlgafloatvec(V)
{
  LVAL x = xlgatvec();
  if (gettvecetype(x) != a_flonum) xlbadtype(x);
  return x;
}

static LVAL xlgafloatmat(V)
{
  LVAL x = xlgadarray();
  x = getdarraydata(x);
  if (! tvecp(x) || gettvecetype(x) != a_flonum) xlbadtype(x);
  return x;
}

static VOID checktvecsize P2C(LVAL, x, int, n)
{
  if (gettvecsize(x) != n) xlbadtype(x);
}

LVAL xsminmaxrelsize(V)
{
  LVAL x, y, s;
  double *dx, *dy, *ds;
  int n;

  x = xlgafloatvec();
  y = xlgafloatvec();
  s = xlgafloatvec();
  xllastarg();

  n = gettvecsize(x);
  checktvecsize(y, n);
  checktvecsize(s, n);
  dx = (double *) gettvecdata(x);
  dy = (double *) gettvecdata(y);
  ds = (double *) gettvecdata(s);
  return cvflonum((FLOTYPE) maxrelsize(n, dx, dy, ds));
}

LVAL xsmincholsolve(V)
{
  int n;
  LVAL g, L, s;
  double *dg, *dL, *ds;

  g = xlgafloatvec();
  L = xlgafloatmat();
  s = xlgafloatvec();
  xllastarg();

  n = gettvecsize(g);
  checktvecsize(L, n * n);
  checktvecsize(s, n);
  dg = (double *) gettvecdata(g);
  dL = (double *) gettvecdata(L);
  ds = (double *) gettvecdata(s);

  cholsolve(n, dg, dL, ds);
  return NIL;
}

LVAL xsminmodelhess(V)
{
  LVAL s, H, L;
  int n;
  double *ds, *dH, *dL, value;

  s = xlgafloatvec();
  H = xlgafloatmat();
  L = xlgafloatmat();
  xllastarg();

  n = gettvecsize(s);
  checktvecsize(H, n * n);
  checktvecsize(L, n * n);
  ds = (double *) gettvecdata(s);
  dH = (double *) gettvecdata(H);
  dL = (double *) gettvecdata(L);

  modelhess(n, ds, dH, dL, &value);
  return cvflonum((FLOTYPE) value);
}

LVAL xsmingradsize(V)
{
  LVAL g, x, s;
  int n;
  double *dg, *dx, *ds, scale;

  g = xlgafloatvec();
  x = xlgafloatvec();
  s = xlgafloatvec();
  scale = makefloat(xlgetarg());
  xllastarg();

  n = gettvecsize(g);
  checktvecsize(x, n);
  checktvecsize(s, n);
  dg = (double *) gettvecdata(g);
  dx = (double *) gettvecdata(x);
  ds = (double *) gettvecdata(s);

  return cvflonum((FLOTYPE) basegradsize(n, dg, dx, ds, scale));
}

LVAL xsminlinesearch(V)
{
  double f, initslope, lambda, newf, lambdaprev, fprev;

  f = makefloat(xlgetarg());
  initslope = makefloat(xlgetarg());
  lambda = makefloat(xlgetarg());
  newf = makefloat(xlgetarg());
  lambdaprev = makefloat(xlgetarg());
  fprev = makefloat(xlgetarg());
  xllastarg();

  return cvflonum((FLOTYPE) linesearch_step(f, initslope, lambda,
					    newf, lambdaprev, fprev));
}
