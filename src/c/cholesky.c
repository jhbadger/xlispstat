/* choldecomp - Cholesky decomposition  routines.                      */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"
#include "xlstat.h"
#include "linalg.h"

/* forward declarations */
LOCAL double Max P2H(double, double);

LOCAL double Max P2C(double, a, double, b)
{
  return(a > b ? a : b);
}

#define Dot(n, x, y) blas_ddot(n, x, 1, y, 1)

VOID choldecomp P4C(double *, a, int, n, double, maxoffl, double *, maxadd)
{
  double minl, minljj, minl2;
  int i, j;
  int in, jn;

  minl = pow(macheps(), 0.25) * maxoffl;
  minl2 = 0.0;
  
  if (maxoffl == 0.0) {
    for (i = 0; i < n; i++) {
      int ii = i * n + i;
      maxoffl = Max(fabs(a[ii]), maxoffl);
    }
    maxoffl = sqrt(maxoffl);
    minl2 = sqrt(macheps()) * maxoffl;
  }
  
  *maxadd = 0.0;
  for (j = 0, jn = 0; j < n; j++, jn += n) {
    int jj = jn + j;
    a[jj] -= Dot(j, a + jn, a + jn);
    
    minljj = 0.0;
    
    for (i = j + 1, in = (j + 1) * n; i < n; i++, in += n) {
      int ij = in + j;
      int ji = jn + i;
      a[ij] = a[ji];
      a[ij] -= Dot(j, a + in, a + jn);
      minljj = Max(fabs(a[ij]), minljj);
    }
    
    minljj = Max(minljj / maxoffl, minl);
    
    if (a[jj] > minljj * minljj) a[jj] = sqrt(a[jj]);
    else {
      if (minljj < minl2) minljj = minl2;
      *maxadd = Max(*maxadd, minljj * minljj - a[jj]);
      a[jj] = minljj;
    }
    
    /**** use BLAS dscal */
    for (i = j + 1, in = (j + 1) * n; i < n; i++, in += n) {
      int ij = in + j;
      a[ij] /= a[jj];
    }
  }
  
  for (i = 0, in = 0; i < n; i++, in += n) {
    for (j = i + 1; j < n; j++) {
      int ij = in + j;
      a[ij] = 0.0;
    }
  }
}
