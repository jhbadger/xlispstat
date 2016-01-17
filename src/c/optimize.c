/* optimimize - basic optimization routines                            */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#ifdef OPTIMIZE

#include "xlisp.h"
#include "xlstat.h"

extern LVAL sk_tolerance, sk_max_iters, k_start, s_true, k_verbose;

/************************************************************************/
/**                                                                    **/
/**                     One Dimensional Search                         **/
/**                                                                    **/
/************************************************************************/

#define GOLD   1.618034
#define GLIMIT 100.0
#ifndef TINY
# define TINY   1.0e-20
#endif
#ifndef HUGE
# define HUGE   1.0e20
#endif
#define Max(a,b)      ((a) > (b) ? (a) : (b))
#define SIGN(a,b)     ((b) > 0.0 ? fabs(a) : -fabs(a))
#define SHFT(a,b,c,d) (a)=(b);(b)=(c);(c)=(d);
#define BRAKMAX 50
#define ZEPS 1.0e-10

static double evfun1 P2C(LVAL, f, double, x)
{
  LVAL tmp;
  
  xlsave1(tmp);
  tmp = cvflonum((FLOTYPE) x);
  tmp = xsfuncall1(f, tmp);
  xlpop();
  return(makefloat(tmp));
}

LVAL xsbracket_search(V)
{
  LVAL f, result, temp;
  double ax, bx, cx, fa, fb, fc, ulim, u, r, q, fu, dum;
  int i, itmax, verbose;
  
  f = xlgetarg();
  ax = makefloat(xlgetarg());
  bx = makefloat(xlgetarg());
  if (xlgetkeyarg(sk_max_iters, &temp)) itmax = getfixnum(temp);
  else itmax = BRAKMAX;
  if (xlgetkeyarg(k_verbose, &temp)) verbose = (temp != NIL);
  else verbose = FALSE;
  
  fa = evfun1(f, ax);
  fb = evfun1(f, bx);
  if (fb > fa) {
    SHFT(dum, ax, bx, dum)
    SHFT(dum, fb, fa, dum)
  }
  cx = bx + GOLD * (bx - ax);
  fc = evfun1(f, cx);
  for (i = 0; i < itmax && fb > fc; i++) {
    if (verbose) {
      sprintf(buf, "a = %f, b = %f, f(a) = %f, f(b) = %f\n",
                   (ax < cx) ? ax : cx, (ax < cx) ? cx : ax,
                   (ax < cx) ? fa : fc, (ax < cx) ? fc : fa);
      stdputstr(buf);
    }
    r = (bx - ax) * (fb - fc);
    q = (bx - cx) * (fb - fa);
    u = bx - ((bx - cx) * q - (bx - ax) * r)
      / (2.0 * SIGN(Max(fabs(q - r), TINY), q - r));
    ulim = bx + GLIMIT * (cx - bx);
    if ((bx - u) * (u - cx) > 0.0) {
      fu = evfun1(f, u);
      if (fu < fc) {
        ax = bx;
        bx = u;
        fa = fb;
        fb = fu;
        break;
      }
      else if (fu > fb) {
        cx = u;
        fc = fu;
        break;
      }
      u = cx + GOLD * (cx - bx);
      fu = evfun1(f, u);
    }
    else if ((cx - u) * (u - ulim) > 0.0) {
      fu = evfun1(f, u);
      if (fu < fc) {
        SHFT(bx, cx, u, cx + GOLD * (cx - bx))
        SHFT(fb, fc, fu, evfun1(f, u))
      }
    }
    else if ((u - ulim) * (ulim - cx) >= 0.0) {
      u = ulim;
      fu = evfun1(f, u);
    }
    else {
      u = cx + GOLD * (cx - bx);
      fu = evfun1(f, u);
    }
    SHFT(ax, bx, cx, u)
    SHFT(fa, fb, fc, fu)
  }
  
  if (ax > cx) {
  	dum = ax; ax = cx; cx = dum;
  	dum = fa; fa = fc; fc = dum;
  }
  
  xlsave1(result);
  result = mklist(2, NIL);
  rplaca(result, mklist(3, NIL));
  rplaca(cdr(result), mklist(3, NIL));
  temp = car(result);
  rplaca(temp, cvflonum((FLOTYPE) ax)); temp = cdr(temp);
  rplaca(temp, cvflonum((FLOTYPE) bx)); temp = cdr(temp);
  rplaca(temp, cvflonum((FLOTYPE) cx));
  temp = car(cdr(result));
  rplaca(temp, cvflonum((FLOTYPE) fa)); temp = cdr(temp);
  rplaca(temp, cvflonum((FLOTYPE) fb)); temp = cdr(temp);
  rplaca(temp, cvflonum((FLOTYPE) fc));
  xlpop();
  
  return(result);
}

#define R 0.61803399
#define C (1.0 - R)
#define GTOL .000001

LVAL xsgolden_search(V)
{
  double ax, bx, cx, tol, f0, f1, f2, f3, x0, x1, x2, x3, xmin, fmin;
  LVAL f, arg, result;
  int verbose;
  
  f = xlgetarg();
  ax = makefloat(xlgetarg());
  cx = makefloat(xlgetarg());
  
  if (xlgetkeyarg(k_start, &arg)) bx = makefloat(arg);
  else bx = ax + C * (cx - ax);
  
  if (xlgetkeyarg(sk_tolerance, &arg)) tol = makefloat(arg);
  else tol = GTOL;
  
  if (xlgetkeyarg(k_verbose, &arg)) verbose = (arg != NIL);
  else verbose = FALSE;

  if (tol < ZEPS) tol = ZEPS;
  
  x0 = ax;
  x3 = cx;
  if (fabs(cx - bx) > fabs(bx - ax)) {
    x1 = bx;
    x2 = bx + C * (cx - bx);
  }
  else {
    x2 = bx;
    x1 = bx - C * (bx - ax);
  }
  f1 = evfun1(f, x1);
  f2 = evfun1(f, x2);
  while (fabs(x3 - x0) > tol * (1.0 + fabs(x1) + fabs(x2))) {
    if (verbose) {
      sprintf(buf, "x = %f, f(x) = %f\n", (f1 < f2) ? x1 : x2, (f1 < f2) ? f1 : f2);
      stdputstr(buf);
    }
    if (f2 < f1) {
      SHFT(x0, x1, x2, R * x1 + C * x3)
      SHFT(f0, f1, f2, evfun1(f, x2))
    }
    else {
      SHFT(x3, x2, x1, R * x2 + C * x0)
      SHFT(f3, f2, f1, evfun1(f, x1))
    }
  }
  if (f1 < f2) { xmin = x1; fmin = f1; }
  else { xmin = x2; fmin = f2; }
  
  xlsave1(result);
  xlpop();
  result = mklist(2, NIL);
  rplaca(result, cvflonum((FLOTYPE) xmin));
  rplaca(cdr(result), cvflonum((FLOTYPE) fmin));
  return(result);
}

#define PARITMAX 100
#define PARTOL .00001
#define CGOLD 0.3819660

LVAL xsparabolic_search(V)
{
  int iter, itmax, verbose;
  double ax, bx, cx, tol, a, b, d, etemp, fu, fv, fw, fx, p, q, r;
  double tol1, tol2, u, v, w, x, xm;
  double e = 0.0;
  LVAL f, arg, result;
  
  d = 0.0;
  
  f = xlgetarg();
  ax = makefloat(xlgetarg());
  cx = makefloat(xlgetarg());
  
  if (xlgetkeyarg(k_start, &arg)) bx = makefloat(arg);
  else bx = ax + CGOLD * (cx - ax);
  
  if (xlgetkeyarg(sk_tolerance, &arg)) tol = makefloat(arg);
  else tol = PARTOL;
  
  if (xlgetkeyarg(k_verbose, &arg)) verbose = (arg != NIL);
  else verbose = FALSE;
  
  if (xlgetkeyarg(sk_max_iters, &arg)) {
    if (! fixp(arg)) xlerror("not an integer", arg);
    itmax = getfixnum(arg);
  }
  else itmax = PARITMAX;
  
  if (tol < TINY) xlfail("torerance is too small");
  
  a = ((ax < cx) ? ax : cx);
  b = ((ax > cx) ? ax : cx);
  x = w = v = bx;
  fw = fv = fx = evfun1(f, x);
  for (iter = 0; iter < itmax; iter++) {
    xm = 0.5 * (a + b);
    tol2 = 2.0 * (tol1 = tol * (1.0 + fabs(x)) + ZEPS);
    if (fabs(x - xm) <= (tol2 - 0.5 * (b - a))) {
      break;
    }
    if (fabs(e) > tol1) {
      r = (x - w) * (fx - fv);
      q = (x - v) * (fx - fw);
      p = (x - v) * q - (x - w) * r;
      q = 2.0 * (q - r);
      if (q > 0.0) p = -p;
      q = fabs(q);
      etemp = e;
      e = d; 
      if (fabs(p) >= fabs(0.5 * q * etemp) || p <= q * (a - x) || p >= q * (b - x))
        d = CGOLD * (e = (x >= xm ? a - x : b - x));
      else {
        d = p / q;
        u = x + d;
        if (u - a < tol2 || b - u < tol2)
          d = SIGN(tol1, xm - x);
      }
    }
    else {
      d = CGOLD * (e = (x >= xm ? a - x : b - x));
    }
    u = (fabs(d) >= tol1 ? x + d : x + SIGN(tol1, d));
    fu = evfun1(f, u);
    if (fu <= fx) {
      if (u >= x) a = x; else b = x;
      SHFT(v, w, x, u)
      SHFT(fv, fw, fx, fu)
    }
    else {
      if (u <  x) a = u; else b = u;
      if (fu <= fw || w == x) {
        v = w;
        w = u;
        fv = fw;
        fw = fu;
      }
      else if (fu <= fv || v == x || v == w) {
        v = u;
        fv = fu;
      }
    }
    if (verbose) {
      sprintf(buf, "x = %f, f(x) = %f, a = %f, b = %f\n", x, fx, a, b);
      stdputstr(buf);
    }
  }
  
  xlsave1(result);
  result = mklist(3, NIL);
  rplaca(result, cvflonum((FLOTYPE) x));
  rplaca(cdr(result), cvflonum((FLOTYPE) fx));
  rplaca(cdr(cdr(result)), cvfixnum((FIXTYPE) iter));
  xlpop();
  
  return(result);
}
#endif /* OPTIMIZE */
