/* xlmath2 - xlisp math functions modified and augmented to correspond */
/* more closely to Common Lisp standard                                */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

/* 7/92 TAA -- modified so that ratios that "overflowed" became flonums.
   also added code so that integers that overflow become flonums when
   NOOVFIXNUM is defined. Complex integers fixed as well */

#include "xlisp.h"

/* The stuff enclosed in NOOVFIXNUM defines an attempt to provide
 * extended precision by having integer arithmetic overflow into
 * floating point when necessary. It is not true extended precision,
 * since subsequent results that fit in the fixed point range are not
 * demoted. The system assumes two's complement integer arithmetic.
 * MAXFIX should contain the largest positive FIXTYPE; MINFIX, the
 * most negative FIXTYPE, is computed as ((- MAXFIX) - 1). Multiplication
 * is done in floating point and compared to the FIXTYPE range. This
 * seems faster than othr methods with floating point hardware.
 * Division only requires a check for -MINFIX / -1 (I think).
 */

#ifndef BIGNUMS
#ifdef NOOVFIXNUM
#define goodisum(x, y, z) (((x) > 0) ? ((y) < (z)) : ! ((y) < (z)))
#define goodidiff(x, y, z) (!(((x < 0) ^ (y < 0)) && ((z < 0) ^ (x < 0))))
#define infixnumrange(x) ((FIXTYPE) MINFIX <= (x) && (x) <= (FIXTYPE) MAXFIX)
#endif

#ifndef MINFIX
#define MINFIX          ((- MAXFIX) - 1)
#endif

/* These definititions used instead of those in XLMATH.C */
/* Somewhat cleaned by by Tom Almy */

#define IN 0
#define FL 1
#define CI 2
#define CF 3
#ifdef BIGNUMS
#define RT 4
#endif

typedef struct {
  int mode;
  FIXTYPE val, crval, cival;
#ifdef BIGNUMS
  FIXTYPE num, denom;
#endif
  FLOTYPE fval, cfrval, cfival;
} Number;

/* Function prototypes */
LOCAL VOID checkizero P1H(FIXTYPE);
LOCAL VOID checkfzero P1H(FLOTYPE);
LOCAL VOID badiop(V);
LOCAL VOID badfop(V);
#ifdef BIGNUMS
LOCAL VOID badrop(V);
LOCAL VOID add_ratios P3H(FIXTYPE, FIXTYPE, Number *);
LOCAL VOID mult_ratios P3H(FIXTYPE, FIXTYPE, Number *);
#endif
LOCAL VOID badcop(V);
LOCAL LVAL readnumber P1H(Number *);
LOCAL VOID setmode P2H(Number *, int);
LOCAL VOID matchmodes P2H(Number *, Number *);
LOCAL LVAL lispnumber P1H(Number *);
LOCAL LVAL binary P1H(int);
LOCAL LVAL logbinary P1H(int);
LOCAL FIXTYPE xget_gcd P2H(FIXTYPE, FIXTYPE);
LOCAL LVAL unary P1H(int);
LOCAL LVAL unary2 P1H(int);
LOCAL LVAL predicate P1H(int);
LOCAL LVAL compare P1H(int);
LOCAL LVAL ccompare P1H(int);
LOCAL double logarithm P3H(FLOTYPE, FLOTYPE, int);

#ifdef applec
/* a gross hack */
VOID MODF(double x, double *y)
{
  extended value;
  modf(x, &value);
  *y = value;
}
#else
#define MODF modf
#endif /* applec */

/* Error checking and messages */

/* checkizero - check for integer division by zero */
LOCAL VOID checkizero P1C(FIXTYPE, iarg)
{
  if (iarg == 0)
  xlfail("illegal zero argument");
}

/* checkfzero - check for floating point division by zero or log of zero */
LOCAL VOID checkfzero P1C(FLOTYPE, farg)
{
  if (farg == 0.0)
  xlfail("illegal zero argument");
}

/* badiop - bad integer operation */
LOCAL VOID badiop(V)
{
  xlfail("bad integer operation");
}

/* badfop - bad floating point operation */
LOCAL VOID badfop(V)
{
  xlfail("bad floating point operation");
}

#ifdef BIGNUMS
/* badrop - bad ratio operation */
LOCAL VOID badrop(V)
{
  xlfail("bad ratio operation");
}
#endif

/* badcop - bad complex number operation */
LOCAL VOID badcop(V)
{
  xlfail("bad complex number operation");
}

/* TAA MOD badarg() removed, using preexisting xlbadtype which
   gives same message */

/* complex - Complex number functions  */

/* TAA Mod--do inline, old test for complexp inline as appropriate */
/*  badcomplex() eliminated since it didn't make sense (other numereric
    types were ok), so returned error is now from xlbadtype */

double d_sign P2C(double *, a, double *, b)
{
  /* from the f2c distribution */
  double x;
  x = *a >= 0 ? *a : - *a;
  return *b >= 0 ? x : -x;
}

double z_abs P1C(dcomplex *, z)
{
  /* from the f2c distribution */
  double real, imag;
  double temp;
  real = z->r;
  imag = z->i;

  if(real < 0)
    real = -real;
  if(imag < 0)
    imag = -imag;
  if(imag > real){
    temp = real;
    real = imag;
    imag = temp;
  }
  if((real+imag) == real)
    return(real);

  temp = imag/real;
  temp = real*sqrt(1.0 + temp*temp);  /*overflow!!*/
  return(temp);
}

VOID z_div P3C(dcomplex *, c, dcomplex *, a, dcomplex *, b)
{
  /* from the f2c distribution */
  double ratio, den;
  double abr, abi;
  dcomplex r;

  if( (abr = b->r) < 0.)
    abr = - abr;
  if( (abi = b->i) < 0.)
    abi = - abi;
  if( abr <= abi ) {
    if(abi == 0)
      xlfail("complex division by zero");
    ratio = b->r / b->i ;
    den = b->i * (1 + ratio*ratio);
    r.r = (a->r*ratio + a->i) / den;
    r.i = (a->i*ratio - a->r) / den;
  }
  else {
    ratio = b->i / b->r ;
    den = b->r * (1 + ratio*ratio);
    r.r = (a->r + a->i*ratio) / den;
    r.i = (a->i - a->r*ratio) / den;
  }
  c->r = r.r, c->i = r.i;
}

VOID z_sqrt P2C(dcomplex *, r, dcomplex *, z)
{
  /* from the f2c distribution */
  double mag;
  dcomplex v;

  if((mag = z_abs(z)) == 0.0)
    v.r = v.i = 0.0;
  else if(z->r > 0) {
    v.r = sqrt(0.5 * (mag + z->r));
    v.i = z->i / v.r / 2;
  }
  else {
    v.i = sqrt(0.5 * (mag - z->r));
    if(z->i < 0)
      v.i = - v.i;
    v.r = z->i / v.i / 2;
  }
  r->r = v.r, r->i = v.i;
}

LOCAL VOID makecomplex P2C(dcomplex *, c, LVAL, x)
{
  if (realp(x)) {
    c->r = makefloat(x);
    c->i = 0.0;
  }
  else if (complexp(x)) {
    c->r = makefloat(getreal(x));
    c->i = makefloat(getimag(x));
  }
  else xlerror("not a number", x);
}

LOCAL double z_phase P1C(dcomplex *, c)
{
  double phi;
  
  if (c->r == 0.0) {
    if (c->i > 0.0) phi = PI / 2;
    else if (c->i == 0.0) phi = 0.0;
    else phi = -PI / 2;
  }
  else phi = atan2(c->i, c->r);
  return(phi);
}

#define cvcomplex(c) newdcomplex((c).r,(c).i)

LOCAL VOID z_exp P2C(dcomplex *, r, dcomplex *, z)
{
  /* from the f2c distribution */
  double expx;

  expx = exp(z->r);
  r->r = expx * cos(z->i);
  r->i = expx * sin(z->i);
}

LOCAL VOID z_log P2C(dcomplex *, r, dcomplex *, z)
{
  /* adapted from the f2c distribution */
  double mod;
  dcomplex v;

  mod = z_abs(z);
  checkfzero(mod);
  v.r = log(mod);
  v.i = atan2(z->i, z->r);
  r->r = v.r, r->i = v.i;
}

LOCAL VOID z_expt P3C(dcomplex *, r, dcomplex *, a, dcomplex *, b)
{
  /* adapted from pow_zz.c in the f2c distribution */
  if (b->r == 0.0 && b->i == 0.0)
    r->r = 1.0, r->i = 0.0;
  else if (a->r == 0.0 && a->i == 0.0)
    r->r = 0.0, r->i = 0.0;
  else {
    double logr, logi, x, y;

    logr = log(z_abs(a));
    logi = atan2(a->i, a->r);

    x = exp(logr * b->r - logi * b->i);
    y = logr * b->i + logi * b->r;

    r->r = x * cos(y);
    r->i = x * sin(y);
  }
}

LOCAL VOID z_sin P2C(dcomplex *, r, dcomplex *, c)
{
  /* Better code by Hume Smith, 11/92 */
  dcomplex val;

  val.r = sin(c->r) * cosh(c->i);
  val.i = cos(c->r) * sinh(c->i);
  r->r = val.r, r->i = val.i;
}

LOCAL VOID z_cos P2C(dcomplex *, r, dcomplex *, c)
{
  /* Better code by Hume Smith, 11/92 */
  dcomplex val;

  val.r =  cos(c->r) * cosh(c->i);
  val.i = -sin(c->r) * sinh(c->i);
  r->r = val.r, r->i = val.i;
}

LOCAL VOID z_tan P2C(dcomplex *, ret_val, dcomplex *, z)
{
  /* Adapted from ctan.f in FN from NETLIB. */
  /* This function does not attempt to signal loss of precision. */
  double x2, y2;
  double den, sn2x;

  x2 = z->r * 2.0;
  y2 = z->i * 2.0;

  sn2x = sin(x2);
  den = cos(x2) + cosh(y2);
  checkfzero(den);
  ret_val->r = sn2x / den,  ret_val->i = sinh(y2) / den;
  return;
}

LOCAL VOID z_asin P2C(dcomplex *, r, dcomplex *, c)
{
  /* Improved code by Hume Smith 11/92 */
  dcomplex sx, val;

  /* compute sx = 1 - c*c */
  sx.r = 1.0 - c->r*c->r + c->i*c->i;
  sx.i = -2.0*c->r*c->i;

  /* compute sx = i*c + sqrt(sx) */
  z_sqrt(&sx, &sx);
  sx.r -= c->i;
  sx.i += c->r;
 
  /* compute val = -i ln(sx) */
  z_log(&sx, &sx);
  val.r =  sx.i;
  val.i = -sx.r;
  r->r = val.r, r->i = val.i;
}

LOCAL VOID z_acos P2C(dcomplex *, r, dcomplex *, c)
{
  /* Improved code by Hume Smith 11/92 */
  dcomplex sx, val;

  /* compute val = 1 - c*c */
  sx.r = 1.0 - c->r*c->r + c->i*c->i;
  sx.i = -2.0*c->r*c->i;
  z_sqrt(&val, &sx);

  /* compute sx = c + i sqrt(val) */
  sx.r = c->r - val.i;
  sx.i = c->i + val.r;
  z_log(&sx, &sx);

  /* compute val = -i ln(sx)) */
  val.r =  sx.i;
  val.i = -sx.r;
  r->r = val.r, r->i = val.i;
}

LOCAL VOID z_atan P2C(dcomplex *, val, dcomplex *, c)
{
  /* This has been redefined in Jan 1989 */
  dcomplex sx, ix, v;

  sx.r = 1.0 - c->i, sx.i =  c->r;
  ix.r = 1.0 + c->i, ix.i = -c->r;
  z_log(&sx, &sx);
  z_log(&ix, &ix);
  sx.r -= ix.r, sx.i -= ix.i;
  ix.r = 0.0, ix.i = 2.0;
  z_div(&v, &sx, &ix);
  val->r = v.r, val->i = v.i;
}

LOCAL VOID z_atan2 P3C(dcomplex *, val, dcomplex *, n, dcomplex *, d)
{
  dcomplex v;

  /* adapted from catan2.f in FN from NETLIB */
  if (z_abs(d) != 0.0) {
    z_div(&v, n, d);
    z_atan(&v, &v);
  }
  else {
    v.i = 0.0;
    if (z_abs(n) != 0.0) {
      double d__1, d__2;
      d__1 = PI * 0.5;
      d__2 = n->r;
      v.r = d_sign(&d__1, &d__2), v.i = 0.0;
    }
    else
      v.r = 0.0;
  }
  val->r = v.r, val->i = v.i;
}

/* Helper functions */

LOCAL LVAL readnumber P1C(Number *, number)
{
  LVAL arg = xlgetarg(), real, imag;

  switch(ntype(arg)) {
  case FIXNUM:
    number->mode = IN;
    number->val = getfixnum(arg);
    break;
  case FLONUM:
    number->mode = FL;
    number->fval = getflonum(arg);
    break;
#ifdef BIGNUMS
  case RATIO:
    number->mode = RT;
    number->num = getfixnum(getnumer(arg));
    number->denom = getfixnum(getdenom(arg));
    break;
#endif
  case COMPLEX:
    real = getreal(arg);
    imag = getimag(arg);
    if (fixp(real)) {
      number->mode = CI;
      number->crval = getfixnum(real);
      number->cival = getfixnum(imag);
    }
    else {
      number->mode = CF;
      number->cfrval = makefloat(real);
      number->cfival = makefloat(imag);
    }
    break;
  default:
    xlerror("not a number", arg);
  }
  return(arg);
}

LOCAL VOID setmode P2C(Number *, x, int, mode)
{
  switch (mode) {
#ifdef BIGNUMS
  case RT:
    if (x->mode != IN) return;
    x->mode = mode;
    x->num = x->val;
    x->denom = 1;
    break;
#endif
  case FL:
    if (x->mode == IN) {
      x->mode = mode;
      x->fval = x->val;
    }
#ifdef BIGNUMS
    else if (x->mode == RT) {
	x->mode = mode;
	x->fval = x->num / (FLOTYPE) x->denom;
    }
#endif
    else return;
    break;
  case CI:
    if (x->mode != IN) return;
    x->mode = mode;
    x->crval = x->val;
    x->cival = 0;
    break;
  case CF:
    switch (x->mode) {
    case IN:
      x->mode = mode;
      x->cfrval = x->val;
      x->cfival = 0.0;
      break;
#ifdef BIGNUMS
    case RT:
      x->mode = mode;
      x->cfrval = x->num / (FLOTYPE) x->denom;
      x->cfival = 0.0;
      break;
#endif
    case FL:
      x->mode = mode;
      x->cfrval = x->fval;
      x->cfival = 0.0;
      break;
    case CI:
      x->mode = mode;
      x->cfrval = x->crval;
      x->cfival = x->cival;
      break;
    }
    break;
  }
}

LOCAL VOID matchmodes P2C(Number *, x, Number *, y)
{
  int mode = x->mode;
  switch (mode) {
  case IN: mode = y->mode; break;
  case FL: if (y->mode == CI || y->mode == CF) mode = CF; break;
#ifdef BIGNUMS
  case CI: if (y->mode == FL || y->mode == CF || y->mode == RT)
      mode = CF;
  break;
#else
  case CI: if (y->mode == FL || y->mode == CF) mode = CF; break;
#endif
  case CF: break;
#ifdef BIGNUMS
  case RT: if (y->mode == CI) mode = CF;
	   else if (y->mode != IN) mode = y->mode;
	   break;
#endif
  }
  if (x->mode != mode) setmode(x, mode);
  if (y->mode != mode) setmode(y, mode);
}

LOCAL LVAL lispnumber P1C(Number *, x)
{
  switch (x->mode) {
  case IN: return(cvfixnum(x->val));
  case FL: return(cvflonum(x->fval));
  case CI: return(newicomplex(x->crval, x->cival));
  case CF: return(newdcomplex(x->cfrval, x->cfival));
#ifdef BIGNUMS
  case RT: return(cvratio (x->num, x->denom));
#endif
  }
  return NIL; /* avoid warning messages */
}

/* TAA Mod to return FIXTYPE */
LOCAL FIXTYPE xget_gcd P2C(FIXTYPE, n, FIXTYPE, m)                 /* euclid's algorith */
{
   FIXTYPE r;

   for (;;) {
      r = m % n;
      if (r == (FIXTYPE) 0)
        break;
      m = n;
      n = r;
   }

   return (n);
}

#ifdef BIGNUMS
LOCAL VOID add_ratios P3C(FIXTYPE, n1, FIXTYPE, d1, Number *, res)
{
    double lcm, nt;
    FIXTYPE n2=res->num, d2=res->denom;

    lcm = d1 * (d2 /(double)(xget_gcd(d1, d2)));
    nt = n1 * (lcm / d1)  + n2 * (lcm / d2);

    if (fabs(nt) > (double)MAXFIX || fabs(lcm) > (double)MAXFIX) {
	res->fval = (n1/(double)d1) + (n2/(double)d2);
	res->mode = FL;
    }
    else {
	res->num = (FIXTYPE) nt;
	res->denom = (FIXTYPE) lcm;
	/* mode is already RT */
    }
    return;
}

LOCAL VOID mult_ratios P3C(FIXTYPE, n1, FIXTYPE, d1, Number *, res)
{
    FIXTYPE gcd;
    FIXTYPE n2=res->num, d2=res->denom;
    double nt, dt;

    /* TAA rewrite from 2.1d -- reduce first, can help fit */
    if (n1 == 0 || n2 == 0) {	/* handle multiply by zero */
	res->val = 0;
	res->mode = IN;
	return;
    }
    if ((gcd=xget_gcd(n1, d2)) > 1) {
	n1 /= gcd;
	d2 /= gcd;
    }
    if ((gcd=xget_gcd(d1, n2)) > 1) {
	d1 /= gcd;
	n2 /= gcd;
    }

    nt = n1 * (double)n2;
    dt = d1 * (double)d2;
    if (fabs(nt) > (double)MAXFIX || fabs(dt) > (double)MAXFIX) {
	res->fval = nt/dt;
	res->mode = FL;
    }
    else {
	res->num = (FIXTYPE) nt;
	res->denom = (FIXTYPE) dt;
	/* mode is already RT */
    }
    return;
}
#endif

LOCAL LVAL binary P1C(int, which)
{
  LVAL larg;
  Number val, arg;
  FIXTYPE rtemp, itemp;
  FLOTYPE frtemp, fitemp;

  if (xlargc == 1 && (which == '-' || which == '/'
#ifdef XLISP_STAT
		      || which == 'd'
#endif /* XLISP_STAT */
		      )) {
    val.mode = IN;
    switch (which) {
    case '-': val.val = 0; break;
#ifdef XLISP_STAT
    case 'd':
#endif /* XLISP_STAT */
    case '/': val.val = 1; break;
    }
  }
  else larg = readnumber(&val);
  while (moreargs()) {
    larg = readnumber(&arg);
    matchmodes(&val, &arg);
    switch (which) {
    case '+':
      switch (val.mode) {
      case IN:
#ifdef NOOVFIXNUM
	rtemp = val.val + arg.val;
        if (goodisum(val.val, arg.val, rtemp))
	  val.val = rtemp;
        else {
	  val.fval = val.val + (double) arg.val;
	  val.mode = FL;
	}
#else
	val.val += arg.val;
#endif /* NOOVFIXNUM */
	break;
      case FL: val.fval  += arg.fval; break;
      case CI:
#ifdef NOOVFIXNUM
	rtemp = val.crval + arg.crval;
	itemp = val.cival + arg.cival;
	if (goodisum(val.crval, arg.crval, rtemp) &&
	    goodisum(val.cival, arg.cival, itemp)) {
	  val.crval = rtemp;
	  val.cival = itemp;
	}
	else {
	  val.cfrval = val.crval+(double)arg.crval;
	  val.cfival = val.cival+(double)arg.cival;
	  val.mode = CF;
        }
#else
	val.crval += arg.crval;
	val.cival += arg.cival;
#endif /* NOOVFIXNUM */
	break;
      case CF: val.cfrval += arg.cfrval; val.cfival += arg.cfival; break;
#ifdef BIGNUMS
      case RT:
	add_ratios (arg.num, arg.denom, &val);
	break;
#endif
      }
      break;
    case '-':
      switch (val.mode) {
      case IN:
#ifdef NOOVFIXNUM
	rtemp = val.val - arg.val;
	if (goodidiff(val.val, arg.val, rtemp))
	  val.val = rtemp;
	else {
	  val.fval = val.val - (double) arg.val;
	  val.mode = FL;
	}
#else
	val.val -= arg.val;
#endif /* NOOVFIXNUM */
	break;
      case FL: val.fval  -= arg.fval; break;
      case CI:
#ifdef NOOVFIXNUM
        rtemp = val.crval - arg.crval;
	itemp = val.cival - arg.cival;
	if (goodidiff(val.crval, arg.crval, rtemp) &&
	    goodidiff(val.cival, arg.cival, itemp)) {
	  val.crval = rtemp;
	  val.cival = itemp;
	}
	else {
	  val.cfrval = val.crval-(double)arg.crval;
	  val.cfival = val.cival-(double)arg.cival;
	  val.mode = CF;
	}
#else
	val.crval -= arg.crval;
	val.cival -= arg.cival;
#endif /* NOOVFIXNUM */
	break;
      case CF: val.cfrval -= arg.cfrval; val.cfival -= arg.cfival; break;
#ifdef BIGNUMS
      case RT:
	 add_ratios (-arg.num, arg.denom, &val);
	 break;
#endif
      }
      break;
    case '*':
      switch (val.mode) {
      case IN:
#ifdef NOOVFIXNUM
	frtemp = val.val * (double)arg.val; /* do math in floating point! */
	if (infixnumrange(frtemp))
	  val.val = (FIXTYPE)frtemp;
	else {
	  val.fval = frtemp;
	  val.mode = FL;
	}
#else
	val.val *= arg.val;
#endif /* NOOVFIXNUM */
	break;
      case FL: val.fval  *= arg.fval; break;
      case CI:
#ifdef NOOVFIXNUM
	frtemp = val.crval * (double)arg.crval - val.cival * (double)arg.cival;
	fitemp = val.cival * (double)arg.crval + val.crval * (double)arg.cival;
	if (infixnumrange(frtemp) && infixnumrange(fitemp)) {
	  val.crval = (FIXTYPE)frtemp;
	  val.cival = (FIXTYPE)fitemp;
	}
	else {
	  val.cfrval = frtemp;
	  val.cfival = fitemp;
	  val.mode = CF;
	}
#else
        rtemp = val.crval * arg.crval - val.cival * arg.cival;
        itemp = val.cival * arg.crval + val.crval * arg.cival;
        val.crval = rtemp; val.cival = itemp;
#endif /* NOOVFIXNUM */
        break;
      case CF:
        frtemp = val.cfrval * arg.cfrval - val.cfival * arg.cfival;
        fitemp = val.cfival * arg.cfrval + val.cfrval * arg.cfival;
        val.cfrval = frtemp; val.cfival = fitemp;
        break;
#ifdef BIGNUMS
      case RT:
	 mult_ratios (arg.num, arg.denom, &val);
	 break;
#endif
      }
      break;
    case '/':
      switch (val.mode) {
      case IN:
        checkizero(arg.val);
#ifdef BIGNUMS
	setmode(&val, RT);
	val.denom = arg.val;
	break;
#else
        if (
#ifdef NOOVFIXNUM
	    (arg.val != -1 || val.val > MINFIX) &&
#endif
	    val.val % arg.val == 0) {
          val.val /= arg.val;
          break;
        }
        else {
          setmode(&val, FL);
          setmode(&arg, FL);
        }
        /* drop through */
#endif
      case FL:
        checkfzero(arg.fval);
        val.fval /= arg.fval;
        break;
      case CI:
        setmode(&val, CF);
        setmode(&arg, CF);
        /* drop through */
      case CF:
	{
	  double ratio,temp;
	  if (fabs(arg.cfrval) > fabs(arg.cfival)) {
            ratio = arg.cfival / arg.cfrval;
            temp = arg.cfrval + ratio*arg.cfival;
            frtemp = (val.cfrval + val.cfival*ratio)/temp;
            fitemp = (val.cfival - val.cfrval*ratio)/temp;
	  }
	  else {
            checkfzero(arg.cfival);
            ratio = arg.cfrval / arg.cfival;
            temp = arg.cfival + ratio*arg.cfrval;
            frtemp = (val.cfrval*ratio + val.cfival)/temp;
            fitemp = (val.cfival*ratio - val.cfrval)/temp;
	  }
	  val.cfrval = frtemp; val.cfival = fitemp;
	  break;
	}
#ifdef BIGNUMS
      case RT:
	checkizero (arg.num);
	mult_ratios (arg.denom, arg.num, &val);
	break;
#endif
      }
      break;
#ifdef XLISP_STAT
    case 'd':
      switch (val.mode) {
      case IN:
        checkizero(arg.val);
        if (
#ifdef NOOVFIXNUM
	    (arg.val != -1 || val.val > MINFIX) &&
#endif
	    val.val % arg.val == 0) {
          val.val /= arg.val;
          break;
        }
        else {
          setmode(&val, FL);
          setmode(&arg, FL);
        }
        /* drop through */
      case FL:
        checkfzero(arg.fval);
        val.fval /= arg.fval;
        break;
      case CI:
        setmode(&val, CF);
        setmode(&arg, CF);
        /* drop through */
      case CF:
	{
	  double ratio,temp;
	  if (fabs(arg.cfrval) > fabs(arg.cfival)) {
            ratio = arg.cfival / arg.cfrval;
            temp = arg.cfrval + ratio*arg.cfival;
            frtemp = (val.cfrval + val.cfival*ratio)/temp;
            fitemp = (val.cfival - val.cfrval*ratio)/temp;
	  }
	  else {
            checkfzero(arg.cfival);
            ratio = arg.cfrval / arg.cfival;
            temp = arg.cfival + ratio*arg.cfrval;
            frtemp = (val.cfrval*ratio + val.cfival)/temp;
            fitemp = (val.cfival*ratio - val.cfrval)/temp;
	  }
	  val.cfrval = frtemp; val.cfival = fitemp;
	  break;
	}
#ifdef BIGNUMS
      case RT:
	checkizero (arg.num);
	mult_ratios (arg.denom, arg.num, &val);
	break;
#endif
      }
      break;
#endif /* XLISP_STAT */
    case 'M':
      switch (val.mode) {
      case IN: val.val  = (val.val > arg.val)   ? val.val  : arg.val;  break;
      case FL: val.fval = (val.fval > arg.fval) ? val.fval : arg.fval; break;
#ifdef BIGNUMS
      case RT:
	if ((val.num * (double)arg.denom) < (arg.num * (double)val.denom)) {
	  val.num = arg.num;
	  val.denom = arg.denom;
	}
	break;
#endif
      default: xlbadtype(larg);
      }
      break;
    case 'm':
      switch (val.mode) {
      case IN: val.val  = (val.val < arg.val)   ? val.val  : arg.val;  break;
      case FL: val.fval = (val.fval < arg.fval) ? val.fval : arg.fval; break;
#ifdef BIGNUMS
      case RT:
	if ((val.num * (double)arg.denom) > (arg.num * (double)val.denom)) {
	  val.num = arg.num;
	  val.denom = arg.denom;
	}
	break;
#endif
      default: xlbadtype(larg);
      }
      break;
    }
  }
  return(lispnumber(&val));
}

/* This has been completely rewritten by Tom Almy to handle floating point
   arguments */
LVAL xrem(V)
{
  Number numer, div;
  double ftemp;

  readnumber(&numer);
  readnumber(&div);
  xllastarg();

  matchmodes(&numer, &div);

  switch (numer.mode) {
  case IN:
    checkizero(div.val);
    return (cvfixnum((FIXTYPE) numer.val % div.val));
  case FL:
    checkfzero(div.fval);
    MODF(numer.fval / div.fval, &ftemp);
    return (cvflonum((FLOTYPE)(numer.fval - ftemp*div.fval)));
#ifdef BIGNUMS
  case RT:
    checkizero(div.num);
    mult_ratios(div.denom, div.num, &numer);
    if (numer.mode == RT) {
      numer.num %= numer.denom;	/* get remainder */
      mult_ratios(div.num, div.denom, &numer);
    }
    else if (numer.mode == FL) {
      numer.fval = numer.num/numer.denom;
      MODF(numer.fval / div.fval, &ftemp);
      numer.fval -= ftemp*div.fval;
    }
    return (lispnumber(&numer));
#endif
  }
  badcop();
  return NIL; /* fool compiler into not giving warning */
}

LVAL xash(V) /* arithmetic shift left */
{
  FIXTYPE arg, val;

  arg = getfixnum(xlgafixnum());
  val = getfixnum(xlgafixnum());
  xllastarg();

  return cvfixnum(val < 0 ? arg >> -val : arg << val);
}

LOCAL LVAL logbinary P1C(int, which)
{
  FIXTYPE val, arg; /* TAA Mod -- was int */

  switch (which) {
  case '&': val = -1; break;
  case '|': val =  0; break;
  case '^': val =  0; break;
  default: val = 0; /* to keep compiler happy */
  }
  while (moreargs()) {
    arg = getfixnum(xlgafixnum());
    switch (which) {
    case '&': val &= arg; break;
    case '|': val |= arg; break;
    case '^': val ^= arg; break;
    }
  }
  return(cvfixnum((FIXTYPE) val));
}

/* binary functions */
/* TAA fix allowing (+) */
LVAL xadd(V)    { return (moreargs()?binary('+'):cvfixnum((FIXTYPE)0)); }
LVAL xsub()    { return (binary('-')); } /* - */
/* TAA fix allowing (*) */
LVAL xmul(V)    { return (moreargs()?binary('*'):cvfixnum((FIXTYPE)1)); }
LVAL xdiv(V)    { return (binary('/')); } /* / */
#ifdef XLISP_STAT
LVAL xfdiv(V)   { return (binary('d')); } /* / */
#endif /* XLISP_STAT */
LVAL xmin(V)    { return (binary('m')); } /* min */
LVAL xmax(V)    { return (binary('M')); } /* max */
LVAL xlogand(V) { return (logbinary('&')); } /* logand */
LVAL xlogior(V) { return (logbinary('|')); } /* logior */
LVAL xlogxor(V) { return (logbinary('^')); } /* logxor */

/* New by Tom Almy */
LVAL xmod(V)
{
  Number numer, div;

  readnumber(&numer);
  readnumber(&div);
  xllastarg();

  matchmodes(&numer, &div);

  switch (numer.mode) {
  case IN:
    checkizero(div.val);
    return(cvfixnum(numer.val -
		    div.val*(FIXTYPE)floor(numer.val/(double)div.val)));
  case FL:
    checkfzero(div.fval);
    return(cvflonum((FLOTYPE)(numer.fval -
			      div.fval*floor((double)(numer.fval/div.fval)))));
#ifdef BIGNUMS
  case RT:
    checkizero(div.num);
    mult_ratios(div.denom, div.num, &numer);
    if (numer.mode == RT) {
      numer.num = numer.num - numer.denom*
	(FIXTYPE)floor(numer.num/(double)numer.denom);
      mult_ratios(div.num, div.denom, &numer);
    }
    else if (numer.mode == FL) {
      numer.fval = numer.num/numer.denom;
      numer.fval -= div.fval*floor((double)(numer.fval/div.fval));
    }
    return (lispnumber(&numer));
#endif
  }
  badcop();
  return NIL; /* fool compiler into not giving warning */
}

LVAL xexpt(V)    /* modified for ratios by TAA */
{
  LVAL base, power;
  int bsign, psign;
  FIXTYPE b, p, val;
  FLOTYPE fb, fp, fval;

  base = xlgetarg();
  power = xlgetarg();
  xllastarg();

  if (fixp(base) && fixp(power)) {
    b = getfixnum(base);
    p = getfixnum(power);
    if (p == 0) return(cvfixnum((FIXTYPE) 1));
    if (b == 0 && p > 0) return(cvfixnum((FIXTYPE) 0));
    checkizero(b);
    if ((bsign = (b < 0)) != 0) b = -b;
    if ((psign = (p < 0)) != 0) p = -p;

    fval = floor(pow((double) b, (double) p) + 0.1); /* to get integer right */
    if (bsign && (p & 1)) fval = -fval;
    if (!psign) {
#ifdef NOOVFIXNUM
      if (infixnumrange(fval)) {
	val = fval;
	return(cvfixnum((FIXTYPE) val));
      }
#else
      val = fval;   /* this method causes errors on intel architectures */
      if (val == fval) return(cvfixnum(val));
#endif /* NOOVFIXNUM */
      else /* to handle precision for large results */
	return(cvflonum((FLOTYPE) fval));
    }
    else {
#ifdef BIGNUMS
      val = fval;
      if (val == fval) return (cvratio(1, val));
      else return (cvflonum((FLOTYPE) 1.0 / fval));
#else
      checkfzero(fval);
      return(cvflonum((FLOTYPE) 1.0 / fval));
#endif
    }
  }
#ifdef BIGNUMS
  else if (ratiop(base) && fixp(power)) {
    FIXTYPE vald;   /* integer of denominator result */
    FLOTYPE fvald;  /* denominator result */
    b = getfixnum(getnumer(base)); /* start with just the numerator */
    p = getfixnum(power);
    if (p == 0) return(cvfixnum((FIXTYPE) 1));
    if ((bsign = (b < 0)) != 0) b = -b;
    if ((psign = (p < 0)) != 0) p = -p;

    fval = floor(pow((double) b, (double) p) + 0.1); /* to get integer right */
    if (bsign && (p & 1)) fval = -fval;

    fvald = floor(pow((double) getfixnum(getdenom(base)), (double) p) + 0.1);

    val = fval;
    vald = fvald;

    if (!psign) {
      if (val == fval && vald == fvald) /* will fit in ratio */
	  return (cvratio(val, vald));
      else return (cvflonum(fval/fvald));
    }
    else {
      if (val == fval && vald == fvald) /* will fit in ratio */
	  return (cvratio(vald, val));
      else return (cvflonum(fvald/fval));
    }
  }
#endif
  else if (floatp(base) && fixp(power)) {
    fb = getflonum(base);
    p = getfixnum(power);
    if (p == 0) return(cvflonum((FLOTYPE) 1.0)); /* TAA MOD - used to return
                                    fixnum 1, but CL says should be flonum */
    if (fb == 0.0 && p > 0) return(cvflonum((FLOTYPE) 0.0));
    checkfzero(fb);
    if ((bsign = (fb < 0.0)) != 0) fb = -fb;
    if ((psign = (p < 0)) != 0) p = -p;
    fval = pow((double) fb, (double) p);
    if (bsign && (p & 1)) fval = -fval;
    if (!psign) return(cvflonum((FLOTYPE) fval));
    else {
      checkfzero(fval);
      return(cvflonum((FLOTYPE) 1.0 / fval));
    }
  }
#ifdef BIGNUMS
  else if (realp(base) && (ratiop(power) || floatp(power)))
#else
  else if (realp(base) && floatp(power))
#endif
  {
    fb = makefloat(base);
#ifdef BIGNUMS
    fp = ratiop(power) ?
	getfixnum(getnumer(power))/(FLOTYPE)getfixnum(getdenom(power)) : getflonum(power);
#else
    fp = getflonum(power);
#endif
    if (fp == 0.0) return(cvflonum((FLOTYPE) 1.0));
    if (fb == 0.0 && fp > 0.0) return(cvflonum((FLOTYPE) 0.0));
    if (fb < 0.0) {
      dcomplex zb, zp, zv;
      makecomplex(&zb, base);
      makecomplex(&zp, power);
      z_expt(&zv, &zb, &zp);
      return(cvcomplex(zv));
    }
    if ((psign = (fp < 0.0)) != 0) fp = -fp;
    fval = pow((double) fb, (double) fp);
    if (!psign) return(cvflonum((FLOTYPE) fval));
    else {
      checkfzero(fval);
      return(cvflonum((FLOTYPE) 1.0 / fval));
    }
  }
  else if (complexp(base) || complexp(power)) {
    dcomplex zb, zp, zv;
    makecomplex(&zb, base);
    makecomplex(&zp, power);
    z_expt(&zv, &zb, &zp);
    return(cvcomplex(zv));
  }
  else xlfail("bad argument type(s)");
  return NIL; /* avoid compiler warnings */
}

/* arc tangent -- Tom Almy */
LVAL xatan(V)
{
  Number numer, denom;
  LVAL lnum, ldenom;
  dcomplex cnum, cdenom, cval;

  lnum = readnumber(&numer);

  if (moreargs()) {
    ldenom = readnumber(&denom);
    xllastarg();
    matchmodes(&numer, &denom);

    switch (numer.mode) {
    case IN:
      numer.fval = numer.val; denom.fval = denom.val;
    case FL:
      return (cvflonum((FLOTYPE)atan2(numer.fval, denom.fval)));
#ifdef BIGNUMS
    case RT:
      return (cvflonum((FLOTYPE)atan2(numer.num/(FLOTYPE)numer.denom,
				      denom.num/(FLOTYPE)denom.denom)));
#endif
    default: /* complex */
      makecomplex(&cnum, lnum);
      makecomplex(&cdenom, ldenom);
      z_atan2(&cval, &cnum, &cdenom);
      return (cvcomplex(cval));
    }
  }
  else {
    switch (numer.mode) {
    case IN:
      numer.fval = numer.val;
    case FL:
      return (cvflonum((FLOTYPE)atan(numer.fval)));
#ifdef BIGNUMS
    case RT:
      return (cvflonum((FLOTYPE)atan(numer.num/(FLOTYPE)numer.denom)));
#endif
    default: /* complex */
      makecomplex(&cnum, lnum);
      z_atan(&cnum, &cnum);
      return (cvcomplex(cnum));
    }
  }
}

/* two argument logarithm */
LOCAL double logarithm P3C(FLOTYPE, x, FLOTYPE, base, int, base_supplied)
{
  double lbase;
  if (x <= 0.0) xlfail("logarithm of a nonpositive number");
  if (base_supplied) {
    if (base <= 0.0) xlfail("logarithm to a nonpositive base");
    else {
      lbase = log(base);
      if (lbase == 0.0) xlfail("logarith to a unit base");
      else return((log(x)/lbase));
    }
  }
  return (log(x));
}

LVAL xlog(V)
{
  LVAL arg, base;
  double fx, fb;
  dcomplex zx, zb, zv;

  arg = xlgetarg();
  if (moreargs()) {
    base = xlgetarg();
    xllastarg();
    if (realp(arg) && realp(base)) {
      fx = makefloat(arg);
      fb = makefloat(base);
      if (fx <= 0.0 || fb <= 0.0) {
	makecomplex(&zx, arg);
	makecomplex(&zb, base);
	z_log(&zx, &zx);
	z_log(&zb, &zb);
	z_div(&zv, &zx, &zb);
        return(cvcomplex(zv));
      }
      else return(cvflonum((FLOTYPE) logarithm(fx, fb, TRUE)));
    }
    else if ((realp(arg) && complexp(base))
             || (complexp(arg) && realp(base))
             || (complexp(arg) && complexp(base))) {
      makecomplex(&zx, arg);
      makecomplex(&zb, base);
      z_log(&zx, &zx);
      z_log(&zb, &zb);
      z_div(&zv, &zx, &zb);
      return(cvcomplex(zv));
    }
    else xlfail("bad argument type(s)");
  }
  else {
    xllastarg();
    if (realp(arg)) {
      fx = makefloat(arg);
      if (fx <= 0.0) {
	makecomplex(&zx, arg);
	z_log(&zx, &zx);
	return(cvcomplex(zx));
      }
      else return(cvflonum((FLOTYPE) logarithm(fx, 0.0, FALSE)));
    }
    else if (complexp(arg)) {
      makecomplex(&zx, arg);
      z_log(&zx, &zx);
      return(cvcomplex(zx));
    }
    else xlfail("bad argument type(s)");
  }
  return NIL; /* avoid compiler warnings */
}

/* xgcd - greatest common divisor */
LVAL xgcd(V)
{
  FIXTYPE m,n;
  LVAL arg;

  if (!moreargs())                  /* check for identity case */
    return (cvfixnum((FIXTYPE)0));
  arg = xlgafixnum();
  n = getfixnum(arg);
  if (n < (FIXTYPE)0) n = -n;           /* absolute value */
  while (moreargs()) {
    arg = xlgafixnum();
    m = getfixnum(arg);
    if (m == 0 || n == 0) xlfail("zero argument");
    if (m < (FIXTYPE)0) m = -m;     /* absolute value */

    n = xget_gcd(n,m);
  }
  return (cvfixnum(n));
}

LVAL xlcm(V)                         /* added by kcw */
{
  LVAL arg;
  FIXTYPE n, m, g;
  double t; /* TAA mod, 7/72, to reduce chance of overflow */

  arg = xlgafixnum();
  n = getfixnum(arg);
  if (!moreargs())  {
     if (n < (FIXTYPE) 0) n = -n;
     return (cvfixnum(n));
  }

  while (moreargs())  {
     arg = xlgafixnum();
     m = getfixnum(arg);
     if ((n == (FIXTYPE) 0) || (m == (FIXTYPE) 0))
        return(cvfixnum(0));

     t = n * (double)m;
     g = xget_gcd(n,m);
     t = t/g;
     if (fabs(t) > (double)MAXFIX) xlfail("lcm larger than maximum fixnum");
     n = (FIXTYPE)t;

  }

  if (n < (FIXTYPE) 0) n = -n;

  return (cvfixnum(n));
}

/* unary - handle unary operations */
LOCAL LVAL unary P1C(int, which)
{
  FLOTYPE fval;
  FIXTYPE ival;
  FIXTYPE itemp;
#ifdef BIGNUMS
  FIXTYPE numer, denom;
#endif
  dcomplex cval;
  LVAL arg, real, imag;
  int mode;

  /* get the argument */
  arg = xlgetarg();
  if (which == 'F' && moreargs()) xlgaflonum();
  xllastarg();

  /* check its type */
  if (fixp(arg)) {
    ival = getfixnum(arg);
    mode = IN;
  }
  else if (floatp(arg)) {
    fval = getflonum(arg);
    mode = FL;
  }
#ifdef BIGNUMS
  else if (ratiop(arg)) {
    numer = getfixnum(getnumer(arg));
    denom = getfixnum(getdenom(arg));
    mode = RT;
  }
#endif
  else if (complexp(arg)) {
    makecomplex(&cval, arg);
    real = getreal(arg);
    imag = getimag(arg);
    if (fixp(getreal(arg))) mode = CI;
    else mode = CF;
  }
  else xlerror("not a number", arg);

  switch (which) {
  case '~':
    if (mode == IN) return(cvfixnum((FIXTYPE) ~ival));
    else badiop();
    break;
  case 'A':
    switch (mode) {
#ifdef NOOVFIXNUM
    case IN:
      itemp = ival < 0 ? -ival : ival;
      if (0 <= itemp) return(cvfixnum((FIXTYPE) itemp));
      else fval = ival;
      /* drop through */
#else
    case IN: return(cvfixnum((FIXTYPE) (ival < 0   ? -ival : ival)));
#endif /* NOOVFIXNUM */
    case FL: return(cvflonum((FLOTYPE) (fval < 0.0 ? -fval : fval)));
    case CI:
    case CF: return(cvflonum((FLOTYPE) z_abs(&cval)));
#ifdef BIGNUMS
    case RT: return(cvratio(numer<0?-numer:numer,denom));
#endif
    }
    break;
  case '+':
    switch (mode) {
    case IN:
      itemp = ival + 1;
#ifdef NOOVFIXNUM
      if (itemp < 0 && ival > 0) return(cvflonum(ival + (FLOTYPE) 1.0));
      /* drop through */
#endif /* NOOVFIXNUM */
      return(cvfixnum((FIXTYPE) ival + 1));
    case FL: return(cvflonum((FLOTYPE) fval + 1.0));
    case CI:
      ival = getfixnum(real);
      itemp = ival + 1;
#ifdef NOOVFIXNUM
      if (itemp < 0 && ival > 0)
	return(newdcomplex(ival + (FLOTYPE) 1.0, (FLOTYPE)getfixnum(imag)));
      /* else drop through */
#endif
      return(newicomplex(itemp, getfixnum(imag)));
    case CF: return(newdcomplex(getflonum(real) + 1.0, getflonum(imag)));
#ifdef BIGNUMS
    case RT:
      if ((numer+(double)denom) > (double)MAXFIX)
	return (cvflonum((FLOTYPE)(1.0+ numer/(double)denom)));
      else
	return(cvratio(numer+denom, denom));
#endif
    }
    break;
  case '-':
    switch (mode) {
    case IN: 
      itemp = ival - 1;
#ifdef NOOVFIXNUM
      if (ival < 0 && itemp > 0)
	return(cvflonum(ival - (FLOTYPE) 1.0));
      /* drop through */
#endif /* NOOVFIXNUM */
      return(cvfixnum((FIXTYPE) itemp));
    case FL: return(cvflonum((FLOTYPE) fval - 1.0));
    case CI:
      ival = getfixnum(real);
      itemp = ival - 1;
#ifdef NOOVFIXNUM
      if (ival < 0 && itemp > 0)
        return(newdcomplex(ival - (FLOTYPE) 1.0, (FLOTYPE)getfixnum(imag)));
      /* else drop through */
#endif /* NOOVFIXNUM */
      return(newicomplex(itemp, getfixnum(imag)));
    case CF: return(newdcomplex(getflonum(real) - 1.0, getflonum(imag)));
#ifdef BIGNUMS
    case RT:
	if ((numer-(double)denom) < (double)-MAXFIX)
	    return (cvflonum((FLOTYPE)(-1.0 + numer/(double)denom)));
	else
	    return(cvratio(numer-denom, denom));
#endif
    }
    break;
  case 'S':
    switch (mode) {
    case IN: return(cvflonum((FLOTYPE) sin((double) ival)));
#ifdef BIGNUMS
    case RT: fval = numer / (FLOTYPE) denom;
#endif
    case FL: return(cvflonum((FLOTYPE) sin((double) fval)));
    case CI:
    case CF:
      z_sin(&cval, &cval);
      return(cvcomplex(cval));
    }
  case 'C':
    switch (mode) {
    case IN: return(cvflonum((FLOTYPE) cos((double) ival)));
#ifdef BIGNUMS
    case RT: fval = numer / (FLOTYPE) denom;
#endif
    case FL: return(cvflonum((FLOTYPE) cos((double) fval)));
    case CI:
    case CF:
      z_cos(&cval, &cval);
      return(cvcomplex(cval));
    }
  case 'T':
    switch (mode) {
    case IN: return(cvflonum((FLOTYPE) tan((double) ival)));
#ifdef BIGNUMS
    case RT: fval = numer / (FLOTYPE) denom;
#endif
    case FL: return(cvflonum((FLOTYPE) tan((double) fval)));
    case CI:
    case CF:
      z_tan(&cval, &cval);
      return(cvcomplex(cval));
    }
  case 'E':
    switch (mode) {
    case IN: return(cvflonum((FLOTYPE) exp((double) ival)));
#ifdef BIGNUMS
    case RT: fval = numer / (FLOTYPE) denom;
#endif
    case FL: return(cvflonum((FLOTYPE) exp((double) fval)));
    case CI:
    case CF:
      z_exp(&cval, &cval);
      return(cvcomplex(cval));
    }
    break;
  case 'R':
    switch (mode) {
    case IN:
      if (ival < 0) {
	makecomplex(&cval, arg);
	z_sqrt(&cval, &cval);
	return(cvcomplex(cval));
      }
      else return(cvflonum((FLOTYPE) sqrt((double) ival)));
#ifdef BIGNUMS
    case RT: fval = numer / (FLOTYPE) denom;
#endif
    case FL:
      if (fval < 0) {
	makecomplex(&cval, arg);
	z_sqrt(&cval, &cval);
	return(cvcomplex(cval));
      }
      else return(cvflonum((FLOTYPE) sqrt(fval)));
    case CI:
    case CF:
      z_sqrt(&cval, &cval);
      return(cvcomplex(cval));
    }
    break;
  case 'F':
    switch (mode) {
    case IN: return (cvflonum((FLOTYPE) ival));
#ifdef BIGNUMS
    case RT: fval = numer / (FLOTYPE) denom;
#endif
    case FL: return (cvflonum((FLOTYPE) fval));
    default: badcop();
    }
    break;
  case 's':
    switch (mode) {
#ifdef BIGNUMS
    case RT: fval = numer / (FLOTYPE) denom; goto l1;
#endif
    case IN:
      fval = ival;
      /* drop through */
    case FL:
#ifdef BIGNUMS
l1:
#endif
      if (fval > 1.0 || fval < -1.0) {
	makecomplex(&cval, arg);
	z_asin(&cval, &cval);
        return(cvcomplex(cval));
      }
      else return(cvflonum((FLOTYPE) asin(fval)));
    case CI:
    case CF:
      z_asin(&cval, &cval);
      return(cvcomplex(cval));
    }
    break;
  case 'c':
    switch (mode) {
#ifdef BIGNUMS
    case RT: fval = numer / (FLOTYPE) denom; goto l2;
#endif
    case IN:
      fval = ival;
      /* drop through */
    case FL:
#ifdef BIGNUMS
l2:
#endif
      if (fval > 1.0 || fval < -1.0) {
	makecomplex(&cval, arg);
	z_acos(&cval, &cval);
        return(cvcomplex(cval));
      }
      else return(cvflonum((FLOTYPE) acos(fval)));
    case CI:
    case CF:
      z_acos(&cval, &cval);
      return(cvcomplex(cval));
    }
    break;
  case 'P':
    switch (mode) {
    case IN: return(cvflonum((FLOTYPE) (ival >= 0) ? 0.0 : PI));
#ifdef BIGNUMS
    case RT: fval = numer / (FLOTYPE) denom;
#endif
    case FL: return(cvflonum((FLOTYPE) (fval >= 0.0) ? 0.0 : PI));
    case CI:
    case CF: return(cvflonum((FLOTYPE) z_phase(&cval)));
    }
    break;
#ifdef BIGNUMS
  case 'X':
    switch (mode) {
    case IN:
    case RT: return arg;
    case FL:
      {
	/* NOTE-- This code is specific to 32 bit signed integer math */
	/****** FIX THIS? */
	double dummy, original=fval;
	int e, sign=FALSE;
	if (fval < 0.0) {
	  sign = TRUE;
	  fval = -fval;
	}
	fval = frexp(fval, &e);
	for (; MODF(fval, &dummy)!=0.0 && e>-30 && fval<=1073741823.; e--)
	  fval *= 2.0;
	if (e >= 0) {
	  if (fabs(original) > MAXFIX) /* won't fit -- return float */
	    return (cvflonum((FLOTYPE)original));
	  else
	    return(cvfixnum((FIXTYPE)original));
	}
	return (cvratio((FIXTYPE)(sign? -dummy : dummy),
			(FIXTYPE)ldexp(1.0, -e)));
      }
    case CI:
    case CF: badcop();
    }
    break;
#endif
  default: xlfail("unsupported operation");
  }
  return NIL; /* avoid compiler warning */
}

/**** need to look at thee for overflow handling */
LOCAL LVAL unary2 P1C(int, which)    /* handle truncate, floor, ceiling, and round */
                                     /* 1 or two arguments */
                                     /* By Tom Almy */
{
#ifdef MULVALS  /* Yep, it all has to be redone for MULVALS! TAA 10/93 */
  Number numer, denom;
  double quotient;
  LVAL lval;

  xlnumresults = 2;   /* Always the case */

  lval = readnumber(&numer);

  if (moreargs()) {   /* two argument version */
    readnumber(&denom);
    xllastarg();
    matchmodes(&numer, &denom);

    switch (numer.mode) {
    case IN:
      checkizero(denom.val);
      quotient = numer.val / (double)denom.val;
      break;
    case FL:
      checkfzero(denom.fval);
      quotient = numer.fval/denom.fval;
      break;
#ifdef BIGNUMS
    case RT:
      checkizero(denom.num);
      quotient = ((double)numer.num/numer.denom)/
	((double)denom.num/denom.denom);
      break;
#endif
    default: badcop();
    }
  }
  else { /* single argument version, denominator is one */
    switch (numer.mode) {
    case IN:
      xlresults[1] = cvfixnum((FIXTYPE) 0);
      return (xlresults[0]=lval); /* done early! */
    case FL: 
      denom.fval = 1.0;
      quotient = numer.fval;
      break;
#ifdef BIGNUMS
    case RT:
      denom.mode = RT;
      denom.num = denom.denom = 1;
      quotient = (double)numer.num/numer.denom;
      break;
#endif
    default: badcop();
    }
  }

  switch (which)  { /* now do it! */
  case 'I': MODF(quotient,&quotient); break;
  case '_': quotient = floor(quotient); break;
  case '^': quotient = ceil(quotient); break;
    /* round is flakey when value is too big for a long --
       then it (probably) rounds the .5 case "randomly" */
  case 'r': quotient = (((((long)quotient & 1)==0)^(quotient < 0))?
			ceil(quotient - 0.5) :
			floor(quotient + 0.5)); break;
  }

  /* calculate remainder in same type as arguments */
  switch (numer.mode) {
#ifdef BIGNUMS
  case RT:
    /* we can't do it using ratios if the quotient is too big */
    if (fabs(quotient) < (double) MAXFIX) {
      mult_ratios((FIXTYPE)quotient, (FIXTYPE)1, &denom);
      switch (denom.mode) {
      case RT:	/* we are still a ratio -- good news! */
	add_ratios(-denom.num, denom.denom, &numer);
	xlresults[1] = cvratio(numer.num, numer.denom);
	break;
      case IN:	/* it has become an integer */
	add_ratios(denom.val, (FIXTYPE) 1, &numer);
	xlresults[1] = lispnumber(&numer);
	break;
      case FL:	/* became floating after multiply --
		   can this really happen??*/
	xlresults[1]=cvflonum((FLOTYPE)
			      ((double)numer.num/numer.denom
			       - denom.fval));
	break;
      }
    }
    else {
      /* quotient is too big -- do it all as floats */
      xlresults[1] = cvflonum((FLOTYPE)
			      ((double)numer.num/numer.denom -
			       quotient*((double)denom.num/denom.denom)));
    }
    break;
#endif
  case IN:
    xlresults[1] = cvfixnum(numer.val - (FIXTYPE)quotient*denom.val);
    break;
  case FL:
    xlresults[1] = cvflonum((FLOTYPE)(numer.fval-quotient*denom.fval));
    break;
  }

  /* quotient is (almost) always an integer, but allow for overflow */
  return (xlresults[0] = (numer.mode==IN || fabs(quotient)<(double)MAXFIX)?
	  cvfixnum((FIXTYPE)quotient):
	  cvflonum((FLOTYPE)quotient));
#else
  Number numer, denom;
  LVAL lval;

  lval = readnumber(&numer);

  if (moreargs()) {   /* two argument version */
    readnumber(&denom);
    xllastarg();
    matchmodes(&numer, &denom);

    switch (numer.mode) {
    case IN:
      checkizero(denom.val);
      numer.fval = numer.val / (double)denom.val;
      break;
    case FL:
      checkfzero(denom.fval);
      numer.fval /= denom.fval;
      break;
#ifdef BIGNUMS
    case RT:
      checkizero(denom.num);
      numer.fval = ((FLOTYPE)numer.num * denom.denom) /
	((FLOTYPE)numer.denom * denom.num);
      break;
#endif
    default: badcop();
    }
  }
  else { /* single argument version */
    switch (numer.mode) {
    case IN: return lval;   /* no-operation */
    case FL: break;         /* continue */
#ifdef BIGNUMS
    case RT: numer.fval = numer.num / (FLOTYPE) numer.denom;
      break;
#endif
    default: badcop();
    }
  }
  switch (which)  { /* now do it! */
  case 'I': MODF(numer.fval,&numer.fval); break;
  case '_': numer.fval = floor(numer.fval); break;
  case '^': numer.fval = ceil(numer.fval); break;
  case 'r': numer.fval = floor(numer.fval + 0.5); break;
  }
  if (fabs(numer.fval) > (double)MAXFIX)
    return cvflonum((FLOTYPE)numer.fval);
  return cvfixnum((FIXTYPE)numer.fval);
#endif /* MULVALS */
}

/* unary functions */
LVAL xlognot(V) { return (unary('~')); } /* lognot */
LVAL xabs(V)    { return (unary('A')); } /* abs */
LVAL xadd1(V)   { return (unary('+')); } /* 1+ */
LVAL xsub1(V)   { return (unary('-')); } /* 1- */
LVAL xsin(V)    { return (unary('S')); } /* sin */
LVAL xcos(V)    { return (unary('C')); } /* cos */
LVAL xtan(V)    { return (unary('T')); } /* tan */
LVAL xexp(V)    { return (unary('E')); } /* exp */
LVAL xsqrt(V)   { return (unary('R')); } /* sqrt */
LVAL xfloat(V)  { return (unary('F')); } /* float */
LVAL xasin(V)   { return (unary('s')); } /* asin */
LVAL xacos(V)   { return (unary('c')); } /* acos */
LVAL xphase(V)  { return (unary('P')); } /* phase */
LVAL xfix(V)    { return (unary2('I')); } /* truncate */
LVAL xfloor(V)  { return (unary2('_')); } /* floor */
LVAL xceil(V)   { return (unary2('^')); } /* ceiling */
LVAL xround(V)  { return (unary2('r')); } /* round */
#ifdef BIGNUMS /* Added by TAA, 7/92 */
/****** needs vectorizing */
LVAL xrational(V) { return (unary('X')); } /* rational(ize) */
#endif

/* predicate - handle a predicate function */
LOCAL LVAL predicate P1C(int, fcn)
{
  FLOTYPE fval;
  FIXTYPE ival;
  LVAL arg;

  /* get the argument */
  arg = xlgetarg();
  xllastarg();

  /* check the argument type */
  if (fixp(arg)) {
    ival = getfixnum(arg);
    switch (fcn) {
    case '-': ival = (ival < 0); break;
    case 'Z': ival = (ival == 0); break;
    case '+': ival = (ival > 0); break;
    case 'E': ival = ((ival & 1) == 0); break;
    case 'O': ival = ((ival & 1) != 0); break;
    default:  badiop();
    }
  }
  else if (floatp(arg)) {
    fval = getflonum(arg);
    switch (fcn) {
    case '-': ival = (fval < 0); break;
    case 'Z': ival = (fval == 0); break;
    case '+': ival = (fval > 0); break;
    default:  badfop();
    }
  }
#ifdef BIGNUMS
  else if (ratiop(arg)) {
    switch (fcn) {
    case '-': ival = (getfixnum(getnumer(arg)) < 0); break;
    case 'Z': ival = FALSE; break;  /* can't be zero! */
    case '+': ival = (getfixnum(getnumer(arg)) > 0); break;
    default:  badrop();
    }
  }
#endif
  else
    xlbadtype(arg);

  /* return the result value */
  return (ival ? s_true : NIL);
}

/* unary predicates */
LVAL xminusp(V) { return (predicate('-')); } /* minusp */
LVAL xzerop(V)  { return (predicate('Z')); } /* zerop */
LVAL xplusp(V)  { return (predicate('+')); } /* plusp */
LVAL xevenp(V)  { return (predicate('E')); } /* evenp */
LVAL xoddp(V)   { return (predicate('O')); } /* oddp */

/* compare - common compare function */
LOCAL LVAL compare P1C(int, fcn)
{
  FIXTYPE icmp,ival,iarg;
  FLOTYPE fcmp,fval,farg;
  LVAL arg;
  int mode;

  /* get the first argument */
  arg = xlgetarg();

  /* set the type of the first argument */
  if (fixp(arg)) {
    ival = getfixnum(arg);
    mode = 'I';
  }
  else if (floatp(arg)) {
    fval = getflonum(arg);
    mode = 'F';
  }
#ifdef BIGNUMS
  else if (ratiop(arg)) {
    fval = getfixnum(getnumer(arg)) / (FLOTYPE) getfixnum(getdenom(arg));
    mode = 'F';
  }
#endif
  else
    xlbadtype(arg);

  /* handle each remaining argument */
  for (icmp = TRUE; icmp && moreargs(); ival = iarg, fval = farg) {

    /* get the next argument */
    arg = xlgetarg();

    /* check its type */
    if (fixp(arg)) {
      switch (mode) {
      case 'I':
	iarg = getfixnum(arg);
	break;
      case 'F':
	farg = (FLOTYPE)getfixnum(arg);
	break;
      }
    }
    else if (floatp(arg)) {
      switch (mode) {
      case 'I':
	fval = (FLOTYPE)ival;
	farg = getflonum(arg);
	mode = 'F';
	break;
      case 'F':
	farg = getflonum(arg);
	break;
      }
    }
#ifdef BIGNUMS
    else if (ratiop(arg)) {
      switch (mode) {
      case 'I':
	fval = (FLOTYPE)ival;
      case 'F':
	farg = getfixnum(getnumer(arg)) / (FLOTYPE) getfixnum(getdenom(arg));
	mode = 'F';
	break;
      }
    }
#endif
    else
      xlbadtype(arg);

    /* compute result of the compare */
    switch (mode) {
    case 'I':
      icmp = ival - iarg;
      switch (fcn) {
      case '<': icmp = (icmp < 0); break;
      case 'L': icmp = (icmp <= 0); break;
      case '=': icmp = (icmp == 0); break;
      case '#': icmp = (icmp != 0); break;
      case 'G': icmp = (icmp >= 0); break;
      case '>': icmp = (icmp > 0); break;
      }
      break;
    case 'F':
      fcmp = fval - farg;
      switch (fcn) {
      case '<': icmp = (fcmp < 0.0); break;
      case 'L': icmp = (fcmp <= 0.0); break;
      case '=': icmp = (fcmp == 0.0); break;
      case '#': icmp = (fcmp != 0.0); break;
      case 'G': icmp = (fcmp >= 0.0); break;
      case '>': icmp = (fcmp > 0.0); break;
      }
      break;
    }
  }

  /* return the result */
  return (icmp ? s_true : NIL);
}

LOCAL LVAL ccompare P1C(int, which)
{
  Number val, arg;
  int icmp;

  switch (which) {
  case '=': icmp = TRUE;  break;
  case '#': icmp = FALSE; break;
  }
  readnumber(&val);
  while (moreargs()) {
    readnumber(&arg);
    matchmodes(&val, &arg);
    switch (which) {
    case '=':
      switch (val.mode) {
      case IN: icmp = icmp && val.val  == arg.val;  break;
      case FL: icmp = icmp && val.fval == arg.fval; break;
      case CI: icmp = icmp && val.crval==arg.crval && val.cival==arg.cival;
	break;
      case CF: icmp = icmp && val.cfrval==arg.cfrval && val.cfival==arg.cfival;
	break;
#ifdef BIGNUMS
      case RT: icmp = icmp && val.num == arg.num && val.denom == arg.denom;
	  break;
#endif
      }
      break;
    case '#':
      switch (val.mode) {
      case IN: icmp = icmp || val.val  != arg.val;  break;
      case FL: icmp = icmp || val.fval != arg.fval; break;
      case CI: icmp = icmp || val.crval!=arg.crval || val.cival!=arg.cival;
	break;
      case CF: icmp = icmp || val.cfrval!=arg.cfrval || val.cfival!=arg.cfival;
	break;
#ifdef BIGNUMS
      case RT: icmp = icmp || val.num != arg.num || val.denom != arg.denom;
	break;
#endif
      }
      break;
    }
  }
  return((icmp) ? s_true : NIL);
}

/* comparison functions */
LVAL xlss(V) { return (compare('<'));  } /* < */
LVAL xleq(V) { return (compare('L'));  } /* <= */
LVAL xequ(V) { return (ccompare('=')); } /* = */
LVAL xneq(V) { return (ccompare('#')); } /* /= */
LVAL xgeq(V) { return (compare('G'));  } /* >= */
LVAL xgtr(V) { return (compare('>'));  } /* > */


/***********************************************************************/
/**                                                                   **/
/**                     Complex Number Functions                      **/
/**                                                                   **/
/***********************************************************************/

LVAL xcomplex(V) /* TAA rewrite so (complex 12.0) => #c(12.0 0.0) as required
                    by CL. */
{
  LVAL real, imag;

  real = xlgetarg();
  if (moreargs()) {
    imag = xlgetarg();
    xllastarg();
    return (newcomplex(real, imag));
  }
  if (rationalp(real)) return (real);
  return (newcomplex(real,cvflonum((FLOTYPE)0.0)));
}

LVAL xconjugate(V)
{
  LVAL arg = xlgetarg();

  xllastarg();
  if (realp(arg)) return(arg);
  if (! complexp(arg)) xlbadtype(arg);
  if (fixp(getreal(arg)) && fixp(getimag(arg)))
    return(newicomplex(getfixnum(getreal(arg)), -getfixnum(getimag(arg))));
  else
    return(newdcomplex(makefloat(getreal(arg)), -makefloat(getimag(arg))));
}

LVAL xrealpart(V)
{
  LVAL arg = xlgetarg();

  xllastarg();
  if (realp(arg)) return(arg);
  if (! complexp(arg)) xlbadtype(arg);
  return(getreal(arg));
}

LVAL ximagpart(V)
{
  LVAL arg = xlgetarg();

  xllastarg();
  switch (ntype(arg)) {
  case FIXNUM:
#ifdef BIGNUMS
  case RATIO:
#endif
    return(cvfixnum((FIXTYPE) 0));
  case FLONUM:
    return(cvflonum((FLOTYPE) 0.0));
  case COMPLEX:
    return(getimag(arg));
  default:
    xlbadtype(arg);
    return(NIL);
  }
}
#endif
