/* mats2 - Linear algebra routines and Multreg Style Sweep             */
/* Operator. Tolerance should be determined from a keyword variable;   */
/* default tolerance from a global variable.                           */ 
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "linalg.h"

LOCAL VOID mkswpmat P7C(int, n,
			int, p,
			double *, x,
			double *, y,
			double *, w,
			double *, sm,
			double *, xmean)
{
  int i, j, k;
  double val, sum_w, xmeani, xmeanj, ymean;
  double *pwk, *pxki, *pxkj, *pyk;
  
  /* find the sum of the weights and the mean of y */
  for (sum_w = 0.0, val = 0.0, k = 0, pyk = y, pwk = w;
       k < n;
       k++, pyk++, pwk++) {
    sum_w += *pwk;
    val += *pyk * *pwk;
  }
  if (sum_w <= 0)
    xlfail("non positive sum of weights");
  ymean = val / sum_w;
  
  /* find the column means */
  for (i = 0; i < p; i++) {
    for (val = 0.0, k = 0, pxki = x + i, pwk = w;
	 k < n;
	 k++, pxki += p, pwk++)
      val += *pxki * *pwk;
    xmean[i] = val / sum_w;
  }

  /* put 1 / sum_w in topleft, means on left, minus means on top */
  sm[0] = 1.0 / sum_w;
  for (i = 0; i < p; i++) {
    sm[i + 1] = -xmean[i];
    sm[(i + 1) * (p + 2)] = xmean[i];
  }
  sm[p + 1] = -ymean;
  sm[(p + 1) * (p + 2)] = ymean;
  
  /* put sums of adjusted cross products in body */
  for (i = 0; i < p; i ++) {
    xmeani = xmean[i];
    for (j = i; j < p; j++) {
      xmeanj = xmean[j];
      for (val = 0.0, k = 0, pxki = x + i, pxkj = x + j, pwk = w;
	   k < n;
	   k++, pxki += p, pxkj += p, pwk++)
	val += (*pxki - xmeani) * (*pxkj - xmeanj) * *pwk;
      sm[(i + 1) * (p + 2) + (j + 1)] = val;
      sm[(j + 1) * (p + 2) + (i + 1)] = val;
    }
    for (val = 0.0, k = 0, pxki = x + i, pyk = y, pwk = w;
	 k < n;
	 k++, pxki += p, pyk++, pwk++)
      val += (*pxki - xmeani) * (*pyk - ymean) * *pwk;
    sm[(i + 1) * (p + 2) + (p + 1)] = val;
    sm[(p + 1) * (p + 2) + (i + 1)] = val;
  }
  for (val = 0.0, k = 0, pyk = y, pwk = w; k < n; k++, pyk++, pwk++)
    val += (*pyk - ymean) * (*pyk - ymean) * *pwk;
  sm[(p + 1) * (p + 2) + (p + 1)] = val;
}

LOCAL int sweepinplace P5C(int, rows,
			   int, cols,
			   double *, a,
			   int, k,
			   double, tol)
{
  int i, j;
  double pivot, *paij, *pak0, *pakk, *paik, *pakj, meps;
  
  if (k < 0 || k >= rows || k >= rows)
    xlfail("index out of range");
  
  meps = macheps();
  if (tol < meps) tol = meps;
  
  pak0 = a + cols * k;
  pakk = pak0 + k;
  pivot = *pakk;
  
  if (pivot > tol || pivot < -tol) {
    for (i = 0, paij = a, paik = a + k; i < rows; i++, paik += cols)
      for (j = 0, pakj = pak0; j < cols; j++, paij++, pakj++)
	if (i != k && j != k) {
	  *paij -= ((*paik * *pakj) / pivot);
	}
    for (i = 0, paik = a + k; i < rows; i++, paik += cols)
      if (i != k)
	*paik /= pivot;
    for (j = 0, pakj = pak0; j < cols; j++, pakj++)
      if (j != k)
	*pakj /= -pivot;
    *pakk = 1.0 / pivot;
    return TRUE;
  }
  else
    return FALSE;
}

LOCAL VOID getsweepdata P2C(int, n, double **, pdx)
{
  LVAL arg, x;
  int size, type;

  arg = xlgetarg();
  x = darrayp(arg) ? getdarraydata(arg) : arg;
  if (! tvecp(x)) xlbadtype(arg);
  size = gettvecsize(x);
  type = gettvectype(x);

  if (size < n)
    xlerror("incompatible size", arg);

  switch(type) {
  case CD_FLOTYPE:
  case CD_DOUBLE:
    break;
  default:
    xlbadtype(arg);
  }

  *pdx = ((double *) gettvecdata(x));
}

/* Built in BASE-MAKE-SWEEP-MATRIX function */
LVAL xsbasemkswpmat(V)
{
  int n, p;
  double *x, *y, *w, *sm, *xmean;
  
  n = getfixnum(xlgafixnum());
  p = getfixnum(xlgafixnum());
  getsweepdata(n * p, &x);
  getsweepdata(n, &y);
  getsweepdata(n, &w);
  getsweepdata((p + 2) * (p + 2), &sm);
  getsweepdata(p, &xmean);
  xllastarg();

  mkswpmat(n, p, x, y, w, sm, xmean);
  return NIL;
}

LVAL xssweepinplace(V)
{
  int rows, cols, k;
  double *a, tol;

  rows = getfixnum(xlgafixnum());
  cols = getfixnum(xlgafixnum());
  getsweepdata(rows * cols, &a);
  k = getfixnum(xlgafixnum());
  tol = makefloat(xlgetarg());

  return sweepinplace(rows, cols, a, k, tol) ? s_true : NIL;
}
