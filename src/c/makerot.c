/* makerotation - Construct rotation from x to y by alpha.             */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "linalg.h"

VOID make_rotation P6C(int, n,
		       double *, rot,
		       double *, x,
		       double *, y,
		       int, use_alpha,
		       double, alpha)
{
  double nx, ny, xy, c, s, cc, cm1, a, b;
  int i, j, in;
  
  for (i = 0, in = 0; i < n; i++, in += n) {
    for (j = 0; j < n; j++)
      rot[in + j] = 0.0;
    rot[in + i] = 1.0;
  }
  
  nx = blas_dnrm2(n, x, 1);
  ny = blas_dnrm2(n, y, 1);
  if (nx == 0.0 || ny == 0.0) return;
  
  blas_dscal(n, 1.0 / nx, x, 1);
  blas_dscal(n, 1.0 / ny, y, 1);

  xy = blas_ddot(n, x, 1, y, 1);

  c = (use_alpha) ? cos(alpha) : xy;
  cc = 1 - c * c;
  s = (use_alpha) ? sin(alpha) : sqrt(cc > 0 ? cc : 0.0);
  cm1 = c - 1.0;

  blas_daxpy(n, -xy, x, 1, y, 1);

  ny = blas_dnrm2(n, y, 1);
  if (ny == 0.0)
    return;
  blas_dscal(n, 1.0 / ny, y, 1);
  
  for (i = 0, in = 0; i < n; i++, in += n) {
    a =   x[i] * cm1 + y[i] * s;
    b = - x[i] * s   + y[i] * cm1;
    for (j = 0; j < n; j++) 
      rot[in + j] = a * x[j] + b * y[j];
    rot[in + i] += 1.0;
  }
}

