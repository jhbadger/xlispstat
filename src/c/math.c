/* math - Elementwise arithmetic functions                             */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"
#include "xlstat.h"

extern LVAL s_standard_division;

/*************************************************************************/
/*************************************************************************/
/**                                                                     **/
/**                 Recursive Vectorized Math Functions                 **/
/**                                                                     **/
/*************************************************************************/
/*************************************************************************/

/* The basic math functions have been modified to operate element-wise   */
/* on compound data. The operation is recursive: if compound data items  */
/* contain compound data items the mapping proceeds down to the next     */
/* level.                                                                */

#define DEFVECFUN(__vf__, __f__) \
LVAL __vf__(V) { \
  switch (xlargc) { \
  case 0: return __f__(); \
  case 1: if (numberp(xlargv[0])) return __f__(); else break; \
  case 2: \
    if (numberp(xlargv[0]) && numberp(xlargv[1])) return __f__(); else break; \
  } \
  return recursive_subr_map_elements(__f__, __vf__);\
}

DEFVECFUN(xsradd, xadd)
DEFVECFUN(xsrsub, xsub)
DEFVECFUN(xsrmul, xmul)
DEFVECFUN(xsrdiv, xdiv)
DEFVECFUN(xsrmin, xmin)
DEFVECFUN(xsrmax, xmax)
DEFVECFUN(xsrrem, xrem)
DEFVECFUN(xsrmod, xmod)
DEFVECFUN(xsrexpt, xexpt)
DEFVECFUN(xsrlog, xlog)

#ifdef BIGNUMS
DEFVECFUN(xsrdenominator, xdenominator)
DEFVECFUN(xsrnumerator, xnumerator)
DEFVECFUN(xsrrational, xrational)

LOCAL DEFVECFUN(xsrfexpt1,xfexpt)
LOCAL DEFVECFUN(xsrfdiv1,xfdiv)

LVAL xsrfexpt(V)
{
  if (null(getvalue(s_standard_division))) return xsrfexpt1();
  else return xsrexpt();
}

LVAL xsrfdiv(V)
{
  if (null(getvalue(s_standard_division))) return xsrfdiv1();
  else return xsrdiv();
}
#endif /* BIGNUMS */

DEFVECFUN(xsrlogand, xlogand)
DEFVECFUN(xsrlogior, xlogior)
DEFVECFUN(xsrlogxor, xlogxor)
DEFVECFUN(xsrlognot, xlognot)

DEFVECFUN(xsrabs, xabs)
DEFVECFUN(xsradd1, xadd1)
DEFVECFUN(xsrsub1, xsub1)
DEFVECFUN(xsrsin, xsin)
DEFVECFUN(xsrcos, xcos)
DEFVECFUN(xsrtan, xtan)
DEFVECFUN(xsrexp, xexp)
DEFVECFUN(xsrsqrt, xsqrt)
DEFVECFUN(xsrfloat, xfloat)
DEFVECFUN(xsrrand, xrand)
DEFVECFUN(xsrasin, xasin)
DEFVECFUN(xsracos, xacos)
DEFVECFUN(xsratan, xatan)
DEFVECFUN(xsrphase, xphase)

DEFVECFUN(xsrfloor, xfloor)
DEFVECFUN(xsrceil, xceil)
DEFVECFUN(xsrfix, xfix)
DEFVECFUN(xsrround, xround)

DEFVECFUN(xsrminusp, xminusp)
DEFVECFUN(xsrzerop, xzerop)
DEFVECFUN(xsrplusp, xplusp)
DEFVECFUN(xsrevenp, xevenp)
DEFVECFUN(xsroddp, xoddp)

DEFVECFUN(xsrlss, xlss)
DEFVECFUN(xsrleq, xleq)
DEFVECFUN(xsrequ, xequ)
DEFVECFUN(xsrneq, xneq)
DEFVECFUN(xsrgeq, xgeq)
DEFVECFUN(xsrgtr, xgtr)

