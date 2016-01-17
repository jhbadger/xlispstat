/* xlmath3 - xlisp math functions modified and augmented to correspond */
/* more closely to Common Lisp standard                                */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"

#ifdef BIGNUMS
/* This code supports rational, bignum and complex arithmetic. It is
 * based on the following assumptions:
 *   
 * 1) Fixnums use the full range of a FIXTYPE in 2's complement, so
 * that overflow of addition and subtraction can be detected by
 * checking signs.
 *   
 * 2) The range of a FIXTYPE is at least [-2^31,2^31-1].  Floating
 * point products of integers with results in this range are exact and
 * are coerced exactly to the right integer by an assignment. For
 * systems with larger FITYPE ranges, BIGNUM arithmetic is used for
 * larger products and the results are demoted to FIXNUM's
 *
 * 3) SFIXMIN <= -1 and SFIXMAX >= 2 so that FIXZERO, FIXONE, FIXTWO,
 * and FIXNEGTWO can be computed withot allocation or GC. Eventually
 * they should be pre-computed in xlinit.c.
 * 
 * The general approach used in this implementation is to code fixnum,
 * flonum and complex flonum operations for efficiency. Other
 * operations are coded for simplicity, with recursion to simpler
 * types used wherever possible.
 *   
 * Internal operations like add2 assume arguments are protected. */

#define IN 0
#define BG 1
#define RT 2
#define FL 3
#define CR 4
#define CF 5

#ifndef MAXFLOFIX
#define MAXFLOFIX  2147483647.0
#endif
#ifndef MINFLOFIX
#define MINFLOFIX -2147483648.0
#endif

#define goodisum(x, y, z) (((x) > 0) ? ((y) < (z)) : ! ((y) < (z)))
#define goodidiff(x, y, z) (!(((x < 0) ^ (y < 0)) && ((z < 0) ^ (x < 0))))
#define infixnumrange(x) (MINFLOFIX <= (x) && (x) <= MAXFLOFIX)

#define FIXZERO    cvfixnum((FIXTYPE)  0)
#define FIXONE     cvfixnum((FIXTYPE)  1)
#define FIXTWO     cvfixnum((FIXTYPE)  2)
#define FIXNEGTWO  cvfixnum((FIXTYPE) -2)

#define fixzerop(x) (fixp(x) && getfixnum(x) == 0)
#define fixonep(x)  (fixp(x) && getfixnum(x) == 1)

#define realpart(x) (complexp(x) ? getreal(x) : x)
#define imagpart(x) (complexp(x) ? getimag(x) : FIXZERO)

#define numerator(x)   (ratiop(x) ? getnumer(x) : x)
#define denominator(x) (ratiop(x) ? getdenom(x) : FIXONE)

#define as_flotype(x) (floatp(x) ? getflonum(x) : makefloat(x))
#define as_bignum(x) \
  (bignump(x) ? x : fixp(x) ? cvtfixbignum(getfixnum(x)) : xlbadtype(x))

#define cvcomplex(c) newdcomplex((c).r,(c).i)

#define lss2(x,y) (compare2(x,y) <  0)
#define leq2(x,y) (compare2(x,y) <= 0)
#define geq2(x,y) (compare2(x,y) >= 0)
#define gtr2(x,y) (compare2(x,y) >  0)
#define neq2(x,y) (! equ2(x, y))


/* Function prototypes */
LOCAL VOID badzero(V);
LOCAL double logarithm P3H(FLOTYPE, FLOTYPE, int);
LOCAL int  zerop P1H(LVAL);
LOCAL int  plusp P1H(LVAL);
LOCAL int  minusp P1H(LVAL);
LOCAL int  evenp P1H(LVAL);
LOCAL int  oddp P1H(LVAL);
LOCAL int  commontype P2H(LVAL, LVAL);
LOCAL int  compare2 P2H(LVAL, LVAL);
LOCAL int  equ2 P2H(LVAL, LVAL);
LOCAL LVAL uminus P1H(LVAL);
LOCAL LVAL add2 P2H(LVAL, LVAL);
LOCAL LVAL sub2 P2H(LVAL, LVAL);
LOCAL LVAL mul2 P2H(LVAL, LVAL);
LOCAL LVAL div2 P2H(LVAL, LVAL);
#ifdef XLISP_STAT
LOCAL LVAL fdiv2 P2H(LVAL, LVAL);
#endif /* XLISP_STAT */
LOCAL LVAL min2 P2H(LVAL, LVAL);
LOCAL LVAL max2 P2H(LVAL, LVAL);
LOCAL VOID rational_idiv2 P5H(LVAL, LVAL, LVAL *, LVAL *, int);
LOCAL VOID flonum_idiv2 P5H(FLOTYPE, FLOTYPE, FLOTYPE *, FLOTYPE *, int);
LOCAL VOID idiv2 P5H(LVAL, LVAL, LVAL *, LVAL *, int);
LOCAL LVAL add1 P1H(LVAL);
LOCAL LVAL sub1 P1H(LVAL);
LOCAL LVAL gcd2 P2H(LVAL, LVAL);

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

/* Error checking for illegal zero argument */
LOCAL VOID badzero(V) { xlfail("illegal zero argument"); }
#define checkzero(x) if (zerop(x)) badzero();
#define checkizero(x) if (x == 0) badzero();
#define checkfzero(x) if (x == 0.0) badzero();


/* Complex number functions  */

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


/* Internal generic arithmetic functions */

LOCAL int zerop P1C(LVAL, x)
{
  switch (ntype(x)) {
  case FIXNUM:  return (getfixnum(x) == 0);
  case RATIO:   return FALSE;
  case BIGNUM:  return zeropbignum(x);
  case FLONUM:  return (getflonum(x) == 0.0);
  case COMPLEX: return (zerop(getreal(x)) && zerop(getimag(x)));
  default:      return FALSE;
  }
}

LOCAL int plusp P1C(LVAL, x)
{
  switch (ntype(x)) {
  case FIXNUM:  return (getfixnum(x) > 0);
  case RATIO:   return plusp(getnumer(x));
  case BIGNUM:  return ! getbignumsign(x);
  case FLONUM:  return (getflonum(x) > 0.0);
  default:      return FALSE;
  }
}

LOCAL int minusp P1C(LVAL, x)
{
  switch (ntype(x)) {
  case FIXNUM:  return (getfixnum(x) < 0);
  case RATIO:   return minusp(getnumer(x));
  case BIGNUM:  return getbignumsign(x);
  case FLONUM:  return (getflonum(x) < 0.0);
  default:      return FALSE;
  }
}

LOCAL int evenp P1C(LVAL, x)
{
  switch (ntype(x)) {
  case FIXNUM:
    return (getfixnum(x) % 2 == 0);
  case BIGNUM:
    {
      FIXTYPE temp;
      LVAL bigtwo, r;
      xlsave1(bigtwo);
      bigtwo = cvtfixbignum((FIXTYPE) 2);
      divbignum(x, bigtwo, &r);
      xlpop();
      return (cvtbigfixnum(r, &temp) && temp == 0);
    }
  default:      return FALSE;
  }
}

LOCAL int oddp P1C(LVAL, x) { return integerp(x) && ! evenp(x); }

LOCAL int commontype P2C(LVAL, x, LVAL, y)
{
  switch (ntype(x)) {
  case FIXNUM:
    switch (ntype(y)) {
    case FIXNUM:  return IN;
    case BIGNUM:  return BG;
    case RATIO:   return RT;
    case FLONUM:  return FL;
    case COMPLEX: return floatp(getreal(y)) ? CF : CR;
    default:      xlbadtype(y);
    }
  case BIGNUM:
    switch (ntype(y)) {
    case FIXNUM:
    case BIGNUM:  return BG;
    case RATIO:   return RT;
    case FLONUM:  return FL;
    case COMPLEX: return floatp(getreal(y)) ? CF : CR;
    default:      xlbadtype(y);
    }
  case RATIO:
    switch (ntype(y)) {
    case FIXNUM:
    case BIGNUM:
    case RATIO:   return RT;
    case FLONUM:  return FL;
    case COMPLEX: return floatp(getreal(y)) ? CF : CR;
    default:      xlbadtype(y);
    }
  case FLONUM:
    switch (ntype(y)) {
    case FIXNUM:
    case BIGNUM:
    case RATIO:  
    case FLONUM:  return FL;
    case COMPLEX: return CF;
    default:      xlbadtype(y);
    }
  case COMPLEX:
    switch (ntype(y)) {
    case FIXNUM:
    case BIGNUM:
    case RATIO:   return floatp(getreal(x)) ? CF : CR;
    case FLONUM:  return CF;
    case COMPLEX: return (floatp(getreal(x)) || floatp(getreal(y))) ? CF : CR;
    default:      xlbadtype(y);
    }
  default: xlbadtype(x);
  }
  return 0; /* to keep compilers happy */
}

LOCAL int compare2 P2C(LVAL, x, LVAL, y)
{
  switch (commontype(x, y)) {
  case IN:
    {
      FIXTYPE xi, yi;
      xi = getfixnum(x);
      yi = getfixnum(y);
      return xi < yi ? -1 : xi > yi ? 1 : 0;
    }
  case BG:
    {
      int val;
      xlstkcheck(2);
      xlprotect(x);
      xlprotect(y);
      x = as_bignum(x);
      y = as_bignum(y);
      val = comparebignum(x, y);
      xlpopn(2);
      return val;
    }
  case RT:
    {
      LVAL nx, dx, ny, dy, a, b;
      int val;
      xlstkcheck(2);
      xlsave(a);
      xlsave(b);
      nx = numerator(x); dx = denominator(x);
      ny = numerator(y); dy = denominator(y);
      a = mul2(nx, dy); b = mul2(ny, dx);
      val = compare2(a, b);
      xlpopn(2);
      return val;
    }
  case FL:
    {       
      FLOTYPE xd, yd;
      xd = as_flotype(x);
      yd = as_flotype(y);
      return xd < yd ? -1 : xd > yd ? 1 : 0;
    }
  default:
    xlbadtype(complexp(x) ? x : y);
    return 0; /* keep compilers happy */
  }
}

LOCAL int equ2 P2C(LVAL, x, LVAL, y)
{
  switch (commontype(x, y)) {
  case IN:
  case BG:
  case RT:
  case FL:
    return compare2(x, y) == 0;
  case CR:
    return equ2(realpart(x),realpart(y)) && equ2(imagpart(x),imagpart(y));
  case CF:
    {
      LVAL rx, ix, ry, iy;
      FLOTYPE rxd, ixd, ryd, iyd;
      rx = realpart(x); rxd = as_flotype(rx);
      ix = imagpart(x); ixd = as_flotype(ix);
      ry = realpart(y); ryd = as_flotype(ry);
      iy = imagpart(y); iyd = as_flotype(iy);
      return rxd == ryd && ixd == iyd;
    }
  default: return 0; /* keep compilers happy */
  }
}

LOCAL LVAL uminus P1C(LVAL, x)
{
  switch (ntype(x)) {
  case FIXNUM:
    {
      FIXTYPE ix;
      ix = getfixnum(x);
      if (ix > MINFIX)      
	return cvfixnum(-ix);
      /* else fall through */
    }
  case BIGNUM:
    {
      FIXTYPE temp;
      xlprot1(x);
      x = as_bignum(x);
      x = copybignum(x, ! getbignumsign(x));
      xlpop();
      return cvtbigfixnum(x, &temp) ? cvfixnum(temp) : x;
    }
  case RATIO:
    {
      LVAL n, y;
      xlsave1(n);
      n = getnumer(x); n = uminus(n);
      y = div2(n, getdenom(x));
      xlpop();
      return y;
    }
  case FLONUM:
    return cvflonum(-getflonum(x));
  case COMPLEX:
    if (floatp(getreal(x))) {
      return newdcomplex(-getflonum(getreal(x)), -getflonum(getimag(x)));
    }
    else {
      LVAL r, i, y;
      xlstkcheck(2);
      xlsave(r);
      xlsave(i);
      r = getreal(x); i = getimag(x);
      r = uminus(r); i = uminus(i);
      y = newcomplex(r, i);
      xlpopn(2);
      return y;
    }
  default: return xlbadtype(x);
  }
}

LOCAL LVAL add2 P2C(LVAL, x, LVAL, y)
{
  /* try for quick return */
  if (fixzerop(x))
    return y;
  if (fixzerop(y))
    return x;

  /* general case */
  switch (commontype(x, y)) {
  case IN:
    {
      FIXTYPE xi, yi, zi;
      xi = getfixnum(x);
      yi = getfixnum(y);
      zi = xi + yi;
      if (goodisum(xi, yi, zi))
	return cvfixnum(zi);
      /* else drop through */
    }
  case BG:
    {
      LVAL z;
      FIXTYPE temp;
      xlstkcheck(2);
      xlprotect(x);
      xlprotect(y);
      x = as_bignum(x);
      y = as_bignum(y);
      z = addsubbignum(x, y, FALSE);
      xlpopn(2);
      return cvtbigfixnum(z, &temp) ? cvfixnum(temp) : z;
    }
  case RT:
    {
      LVAL nx, dx, ny, dy, n, d, z;
      xlstkcheck(4);
      xlsave(nx);
      xlsave(ny);
      xlsave(n);
      xlsave(d);
      nx = numerator(x); dx = denominator(x);
      ny = numerator(y); dy = denominator(y);
      nx = mul2(nx, dy);
      ny = mul2(ny, dx);
      n = add2(nx, ny);
      d = mul2(dx, dy);
      z = div2(n, d);
      xlpopn(4);
      return z;
    }
  case FL:
    {
      FLOTYPE xd, yd;
      xd = as_flotype(x);
      yd = as_flotype(y);
      return cvflonum(xd + yd);
    }
  case CR:
    {
      LVAL rx, ix, ry, iy, rz, iz, z;
      xlstkcheck(2);
      xlsave(rz);
      xlsave(iz);
      rx = realpart(x); ry = realpart(y); rz = add2(rx, ry);
      ix = imagpart(x); iy = imagpart(y); iz = add2(ix, iy);
      z = newcomplex(rz, iz);
      xlpopn(2);
      return z;
    }
  case CF:
    {
      LVAL rx, ix, ry, iy;
      FLOTYPE rxd, ixd, ryd, iyd;
      rx = realpart(x); rxd = as_flotype(rx);
      ix = imagpart(x); ixd = as_flotype(ix);
      ry = realpart(y); ryd = as_flotype(ry);
      iy = imagpart(y); iyd = as_flotype(iy);
      return newdcomplex(rxd + ryd, ixd + iyd);
    }
  default: return NIL; /* to keep compiler happy */
  }
}

LOCAL LVAL sub2 P2C(LVAL, x, LVAL, y)
{
  /* try for quick exit */
  if (fixzerop(y))
    return x;
  if (fixzerop(x))
    return uminus(y);

  /* general case */
  switch (commontype(x, y)) {
  case IN:
    {
      FIXTYPE xi, yi, zi;
      xi = getfixnum(x);
      yi = getfixnum(y);
      zi = xi - yi;
      if (goodidiff(xi, yi, zi))
	return cvfixnum(zi);
      /* else drop through */
    }
  case BG:
    {
      LVAL z;
      FIXTYPE temp;
      xlstkcheck(2);
      xlprotect(x);
      xlprotect(y);
      x = as_bignum(x);
      y = as_bignum(y);
      z = addsubbignum(x, y, TRUE);
      xlpopn(2);
      return cvtbigfixnum(z, &temp) ? cvfixnum(temp) : z;
    }
  case RT:
    {
      LVAL nx, dx, ny, dy, n, d, z;
      xlstkcheck(4);
      xlsave(nx);
      xlsave(ny);
      xlsave(n);
      xlsave(d);
      nx = numerator(x); dx = denominator(x);
      ny = numerator(y); dy = denominator(y);
      nx = mul2(nx, dy);
      ny = mul2(ny, dx);
      n = sub2(nx, ny);
      d = mul2(dx, dy);
      z = div2(n, d);
      xlpopn(4);
      return z;
    }
  case FL:
    {
      FLOTYPE xd, yd;
      xd = as_flotype(x);
      yd = as_flotype(y);
      return cvflonum(xd - yd);
    }
  case CR:
    {
      LVAL rx, ix, ry, iy, rz, iz, z;
      xlstkcheck(2);
      xlsave(rz);
      xlsave(iz);
      rx = realpart(x); ry = realpart(y); rz = sub2(rx, ry);
      ix = imagpart(x); iy = imagpart(y); iz = sub2(ix, iy);
      z = newcomplex(rz, iz);
      xlpopn(2);
      return z;
    }
  case CF:
    {
      LVAL rx, ix, ry, iy;
      FLOTYPE rxd, ixd, ryd, iyd;
      rx = realpart(x); rxd = as_flotype(rx);
      ix = imagpart(x); ixd = as_flotype(ix);
      ry = realpart(y); ryd = as_flotype(ry);
      iy = imagpart(y); iyd = as_flotype(iy);
      return newdcomplex(rxd - ryd, ixd - iyd);
    }
  default: return NIL; /* to keep compiler happy */
  }
}

LOCAL LVAL mul2 P2C(LVAL, x, LVAL, y)
{
  /* try for quick exit */
  if (fixonep(x))
    return y;
  if (fixonep(y))
    return x;
  if (fixzerop(x) && (rationalp(y) || (complexp(y) && rationalp(getreal(y)))))
      return FIXZERO;
  if (fixzerop(y) && (rationalp(x) || (complexp(x) && rationalp(getreal(x)))))
      return FIXZERO;

  /* general case */
  switch (commontype(x, y)) {
  case IN:
    {
      double xd, yd, zd;
      xd = (double) getfixnum(x);
      yd = (double) getfixnum(y);
      zd = xd * yd;
      if (infixnumrange(zd))
	return cvfixnum((FIXTYPE) zd);
      /* else drop through */
    }
  case BG:
    {
      LVAL z;
      FIXTYPE temp;
      xlstkcheck(2);
      xlprotect(x);
      xlprotect(y);
      x = as_bignum(x);
      y = as_bignum(y);
      z = multbignum(x, y);
      xlpopn(2);
      return cvtbigfixnum(z, &temp) ? cvfixnum(temp) : z;
    }
  case RT:
    {
      LVAL nx, dx, ny, dy, n, d, z;
      xlstkcheck(2);
      xlsave(n);
      xlsave(d);
      nx = numerator(x); dx = denominator(x);
      ny = numerator(y); dy = denominator(y);
      n = mul2(nx, ny);
      d = mul2(dx, dy);
      z = div2(n, d);
      xlpopn(2);
      return z;
    }
  case FL:
    {
      FLOTYPE xd, yd;
      xd = as_flotype(x);
      yd = as_flotype(y);
      return cvflonum(xd * yd);
    }
  case CR:
    {
      LVAL rx, ix, ry, iy, tmp1, tmp2, rz, iz, z;
      xlstkcheck(4);
      xlsave(tmp1);
      xlsave(tmp2);
      xlsave(rz);
      xlsave(iz);
      rx = realpart(x); ix = imagpart(x);
      ry = realpart(y); iy = imagpart(y);
      tmp1 = mul2(rx, ry); tmp2 = mul2(ix, iy); rz = sub2(tmp1, tmp2);
      tmp1 = mul2(ix, ry); tmp2 = mul2(rx, iy); iz = add2(tmp1, tmp2);
      z = newcomplex(rz, iz);
      xlpopn(4);
      return z;
    }
  case CF:
    {
      LVAL rx, ix, ry, iy;
      FLOTYPE rxd, ixd, ryd, iyd;
      rx = realpart(x); rxd = as_flotype(rx);
      ix = imagpart(x); ixd = as_flotype(ix);
      ry = realpart(y); ryd = as_flotype(ry);
      iy = imagpart(y); iyd = as_flotype(iy);
      return newdcomplex(rxd * ryd - ixd * iyd, ixd * ryd + rxd * iyd);
    }
  default: return NIL; /* to keep compiler happy */
  }
}

LOCAL LVAL div2 P2C(LVAL, x, LVAL, y)
{
  checkzero(y);

  /* try quick exit */
  if (fixonep(y))
    return x;
  if (fixzerop(x) && (rationalp(y) || (complexp(y) && rationalp(getreal(y)))))
      return FIXZERO;

  /* general case */
  switch (commontype(x, y)) {
  case IN:
    return cvratio(getfixnum(x), getfixnum(y));
  case BG:
    {
      LVAL z;
      xlstkcheck(2);
      xlprotect(x);
      xlprotect(y);
      x = as_bignum(x);
      y = as_bignum(y);
      z = cvbratio(x, y);
      xlpopn(2);
      return z;
    }
  case RT:
    {
      LVAL nx, dx, ny, dy, n, d, z;
      xlstkcheck(2);
      xlsave(n);
      xlsave(d);
      nx = numerator(x); dx = denominator(x);
      ny = numerator(y); dy = denominator(y);
      n = mul2(nx, dy);
      d = mul2(dx, ny);
      z = div2(n, d);
      xlpopn(2);
      return z;
    }
  case FL:
    {
      FLOTYPE xd, yd;
      xd = as_flotype(x);
      yd = as_flotype(y);
      return cvflonum(xd / yd);
    }
  case CR:
    {
      LVAL rx, ix, ry, iy, tmp1, tmp2, rz, iz, d, z;
      xlstkcheck(5);
      xlsave(tmp1);
      xlsave(tmp2);
      xlsave(d);
      xlsave(rz);
      xlsave(iz);
      rx = realpart(x); ix = imagpart(x);
      ry = realpart(y); iy = imagpart(y);
      tmp1 = mul2(ry, ry); tmp2 = mul2(iy, iy); d = add2(tmp1, tmp2);
      tmp1 = mul2(rx, ry); tmp2 = mul2(ix, iy); tmp1 = add2(tmp1, tmp2);
      rz = div2(tmp1, d);
      tmp1 = mul2(ix, ry); tmp2 = mul2(rx, iy); tmp1 = sub2(tmp1, tmp2);
      iz = div2(tmp1, d);
      z = newcomplex(rz, iz);
      xlpopn(5);
      return z;
    }
  case CF:
    {
      LVAL rx, ix, ry, iy;
      dcomplex cx, cy, cz;
      rx = realpart(x); cx.r = as_flotype(rx);
      ix = imagpart(x); cx.i = as_flotype(ix);
      ry = realpart(y); cy.r = as_flotype(ry);
      iy = imagpart(y); cy.i = as_flotype(iy);
      z_div(&cz, &cx, &cy);
      return newdcomplex(cz.r, cz.i);
    }
  default: return NIL; /* to keep compiler happy */
  }
}

#ifdef XLISP_STAT
LOCAL LVAL fdiv2 P2C(LVAL, x, LVAL, y)
{
  switch (commontype(x, y)) {
  case IN:
    {
      FIXTYPE xi, yi;
      xi = getfixnum(x);
      yi = getfixnum(y);
      checkizero(yi);
      if (yi == -1 && xi == MINFIX)
	return cvratio(xi, yi);
      else if (xi % yi == 0)
	return cvfixnum(xi / yi);
      else
	return cvflonum(xi / (FLOTYPE) yi);
    }
  case BG:
  case RT:
    {
      LVAL z;
      z = div2(x, y);
      if (ratiop(z))
	z = cvflonum(makefloat(z));
      return z;
    }
  case FL:
    {
      FLOTYPE xd, yd;
      xd = as_flotype(x);
      yd = as_flotype(y);
      checkfzero(yd);
      return cvflonum(xd / yd);
    }
  case CR:
    {
      LVAL z, rz, iz;
      z = div2(x,y);
      rz = realpart(z); iz = imagpart(z);
      if (ratiop(rz) || ratiop(iz))
	z = newdcomplex(makefloat(rz), makefloat(iz));
      return z;
    }      
  case CF:
    {
      LVAL rx, ix, ry, iy;
      dcomplex cx, cy, cz;
      rx = realpart(x); cx.r = as_flotype(rx);
      ix = imagpart(x); cx.i = as_flotype(ix);
      ry = realpart(y); cy.r = as_flotype(ry);
      iy = imagpart(y); cy.i = as_flotype(iy);
      z_div(&cz, &cx, &cy);
      return newdcomplex(cz.r, cz.i);
    }
  default: return NIL; /* to keep compiler happy */
  }
}
#endif /* XLISP_STAT */

LOCAL LVAL min2 P2C(LVAL, x, LVAL, y)
{
  switch (commontype(x, y)) {
  case IN: return (getfixnum(x) <= getfixnum(y)) ? x : y;
  case FL: return (as_flotype(x) <= as_flotype(y)) ? x : y;
  default: return leq2(x, y) ? x : y;
  }
}

LOCAL LVAL max2 P2C(LVAL, x, LVAL, y)
{
  switch (commontype(x, y)) {
  case IN: return (getfixnum(x) >= getfixnum(y)) ? x : y;
  case FL: return (as_flotype(x) >= as_flotype(y)) ? x : y;
  default: return geq2(x, y) ? x : y;
  }
}

LOCAL VOID rational_idiv2 P5C(LVAL, x, LVAL, y,
			      LVAL *, pn, LVAL *, pr,
			      int, which)
{
  LVAL n, r;

  checkzero(y);

  if (fixonep(y) && integerp(x)) { /* check for quick exit */
    n = x;
    r = FIXZERO;
  }
  else {
    xlstkcheck(2);
    xlsave(n);
    xlsave(r);

    if (integerp(x) && integerp(y)) { /* base case */
      LVAL xb, yb;
      FIXTYPE temp;

      xlstkcheck(2);
      xlsave(xb);
      xlsave(yb);
      xb = as_bignum(x);
      yb = as_bignum(y);
      n = divbignum(xb, yb, &r);
      xlpopn(2);

      if (cvtbigfixnum(n, &temp))
	n = cvfixnum(temp);
      if (cvtbigfixnum(r, &temp))
	r = cvfixnum(temp);
    }
    else {
      LVAL xn, xd, yn, yd, tmp1, tmp2;

      xn = numerator(x); xd = denominator(x);
      yn = numerator(y); yd = denominator(y);

      xlstkcheck(2);
      xlsave(tmp1);
      xlsave(tmp2);
      tmp1 = mul2(xn, yd);
      tmp2 = mul2(yn, xd);
      rational_idiv2(tmp1, tmp2, &n, &r, 'I');
      tmp1 = mul2(xd, yd);
      r = div2(r, tmp1);
      xlpopn(2);
    }

    /* adjust ratio and remainder if necessary */
    if (which != 'I' && ! zerop(r)) {
      switch (which) {
      case '^':
	if ((plusp(r) && plusp(y)) || (minusp(r) && minusp(y))) {
	  n = add1(n);
	  r = sub2(r, y);
	}
	break;
      case '_':
	if ((minusp(r) && plusp(y)) || (plusp(r) && minusp(y))) {
	  n = sub1(n);
	  r = add2(r, y);
	}
	break;
      case 'r': /* rounds away from zero on ties, not towards even number */
	{
	  LVAL cut;
	  xlsave1(cut);
	  if (plusp(y)) {
	    if (plusp(r)) {
	      cut = div2(y, FIXTWO);
	      if (geq2(r, cut)) { n = add1(n); r = sub2(r, y); }
	    }
	    else {
	      cut = div2(y, FIXNEGTWO);
	      if (leq2(r, cut)) { n = sub1(n); r = add2(r, y); }
	    }
	  }
	  else {
	    if (plusp(r)) {
	      cut = div2(y, FIXNEGTWO);
	      if (geq2(r, cut)) { n = sub1(n); r = add2(r, y); }
	    }
	    else {
	      cut = div2(y, FIXTWO);
	      if (leq2(r, cut)) { n = add1(n); r = sub2(r, y); }
	    }
	  }
	  xlpop();
	}
	break;
      }
    }

    xlpopn(2);
  }
  *pn = n;
  *pr = r;
}

LOCAL VOID flonum_idiv2 P5C(FLOTYPE, xd, FLOTYPE, yd,
			    FLOTYPE *, pnd, FLOTYPE *, prd,
			    int, which)
{
  FLOTYPE nd = *pnd, rd = *prd;

  checkfzero(yd);

  MODF(xd / yd, &nd);
  rd = xd - nd * yd;
  if (rd != 0.0) {
    switch (which) {
    case '^':
      if ((rd > 0.0 && yd > 0.0) || (rd < 0.0 && yd < 0.0)) {
	nd++;
	rd = rd - yd;
      }
      break;
    case '_':
      if ((rd < 0.0 && yd > 0.0) || (rd > 0.0 && yd < 0.0)) {
	nd--;
	rd = rd + yd;
      }
      break;
    case 'r': /* rounds away from zero on ties, not towards even number */
      if (yd > 0.0) {
	if (rd > 0.0) {
	  if (rd >= 0.5 * yd) { nd++; rd -= yd; }
	}
	else {
	  if (rd <= -0.5 * yd) { nd--; rd += yd; }
	}
      }
      else {
	if (rd > 0.0) {
	  if (rd >= -0.5 * yd) { nd--; rd += yd; }
	}
	else {
	  if (rd <= 0.5 * yd) { nd++; rd -= yd; }
	}
      }
      break;
    }
  }
  *pnd = nd;
  *prd = rd;
}

LOCAL VOID idiv2 P5C(LVAL, x, LVAL, y, LVAL *, pn, LVAL *, pr, int, which)
{
  LVAL n, r;

  switch (commontype(x, y)) {
  case IN:
    {
      FIXTYPE xi, yi;
      xi = getfixnum(x);
      yi = getfixnum(y);
      if (yi == 1) { /* check for quick exit */
	n = x;
	r = FIXZERO;
	break;
      }
      else if (xi > MINFIX && xi < MAXFIX && yi > MINFIX && yi < MAXFIX) {
	FIXTYPE ni, ri;

	checkizero(yi);

	ni = xi / yi;
	ri = xi % yi;

	/* adjust ratio and remainder if necessary */
	if (ri != 0) {
	  switch (which) {
	  case '^':
	    if ((ri > 0 && yi > 0) || (ri < 0 && yi < 0)) {
	      ni++;
	      ri = ri - yi;
	    }
	    break;
	  case '_':
	    if ((ri < 0 && yi > 0) || (ri > 0 && yi < 0)) {
	      ni--;
	      ri = ri + yi;
	    }
	    break;
	  case 'r':/* rounds away from zero on ties, not towards even number */
	    if (yi > 0) {
	      if (ri > 0) {
		if (ri >= (yi + 1) / 2) { ni++; ri -= yi; }
	      }
	      else {
		if (ri <= (-yi - 1) / 2) { ni--; ri += yi; }
	      }
	    }
	    else {
	      if (ri > 0) {
		if (ri >= (-yi + 1) / 2) { ni--; ri += yi; }
	      }
	      else {
		if (ri <= (yi - 1) / 2) { ni++; ri -= yi; }
	      }
	    }
	    break;
	  }
	}

	/* only allocate if actually needed */
	if (pn) {
	  xlsave1(n);  /* protect n while allocating r */
	  n = cvfixnum(ni);
	  r = cvfixnum(ri);
	  xlpop();
	}
	else {
	  n = NIL;
	  r = cvfixnum(ri);
	}
	break;
      }
      /* else drop through */
    }
  case BG:
  case RT:
    rational_idiv2(x, y, &n, &r, which);
    break;
  case FL:
    {
      FLOTYPE xd, yd, nd, rd;

      xd = as_flotype(x);
      yd = as_flotype(y);

      flonum_idiv2(xd, yd, &nd, &rd, which);

      /* only allocate n if actually needed */
      if (pn) {
	xlsave1(n);  /* protect n while allocating r */
	if (infixnumrange(nd))
	  n = cvfixnum((FIXTYPE) nd);
	else {
	  FIXTYPE temp;
	  n = cvtflobignum(nd); 
	  if (cvtbigfixnum(n, &temp))
	    n = cvfixnum(temp); /* not needed for 32-big FIXTYPE's */
	}
	r = cvflonum(rd);
	xlpop();
      }
      else {
	n = NIL;
	r = cvflonum(rd);
      }
      break;
    }
  default:
    xlbadtype(complexp(x) ? x : y);
    r = n = NIL; /* fool compiler into not giving warning */
  }
  if (pn) *pn = n;
  if (pr) *pr = r;
}

LOCAL LVAL add1 P1C(LVAL, x)
{
  switch (ntype(x)) {
  case FIXNUM:
    {
      FIXTYPE xi, yi;
      xi = getfixnum(x);
      yi = xi + 1;
      if (goodisum(xi, 1, yi))
	return cvfixnum(yi);
      /* else drop through */
    }
  case BIGNUM:
    {
      LVAL y;
      FIXTYPE temp;
      xlprot1(x);
      x = as_bignum(x);
      y = addsubbignum(x, n_bigmone, TRUE);
      y = cvtbigfixnum(y, &temp) ? cvfixnum(temp) : y;
      xlpop();
      return y;
    }
  case RATIO:
    {
      LVAL nx, dx, y;
      xlsave1(nx);
      nx = numerator(x); dx = denominator(x);
      nx = add2(nx, dx);
      y = div2(nx, dx);
      xlpop();
      return y;
    }
  case FLONUM:
    return cvflonum(as_flotype(x) + 1.0);
  case COMPLEX:
    if (rationalp(getreal(x)) && rationalp(getimag(x))) {
      LVAL ry, y;
      xlsave1(ry);
      ry = add2(realpart(x), FIXONE);
      y = newcomplex(ry, imagpart(x));
      xlpop();
      return y;
    }
    else {
      LVAL rx, ix;
      FLOTYPE ixd, rxd;
      rx = realpart(x); rxd = as_flotype(rx);
      ix = imagpart(x); ixd = as_flotype(ix);
      return newdcomplex(rxd + 1.0, ixd);
    }
  default: return NIL; /* to keep compiler happy */
  }
}

LOCAL LVAL sub1 P1C(LVAL, x)
{
  switch (ntype(x)) {
  case FIXNUM:
    {
      FIXTYPE xi, yi;
      xi = getfixnum(x);
      yi = xi - 1;
      if (goodidiff(xi, 1, yi))
	return cvfixnum(yi);
      /* else drop through */
    }
  case BIGNUM:
    {
      LVAL y;
      FIXTYPE temp;
      xlprot1(x);
      x = as_bignum(x);
      y = addsubbignum(x, n_bigmone, FALSE);
      y = cvtbigfixnum(y, &temp) ? cvfixnum(temp) : y;
      xlpop();
      return y;
    }
  case RATIO:
    {
      LVAL nx, dx, y;
      xlsave1(nx);
      nx = numerator(x); dx = denominator(x);
      nx = sub2(nx, dx);
      y = div2(nx, dx);
      xlpop();
      return y;
    }
  case FLONUM:
    return cvflonum(as_flotype(x) - 1.0);
  case COMPLEX:
    if (rationalp(getreal(x)) && rationalp(getimag(x))) {
      LVAL ry, y;
      xlsave1(ry);
      ry = sub2(realpart(x), FIXONE);
      y = newcomplex(ry, imagpart(x));
      xlpop();
      return y;
    }
    else {
      LVAL rx, ix;
      FLOTYPE ixd, rxd;
      rx = realpart(x); rxd = as_flotype(rx);
      ix = imagpart(x); ixd = as_flotype(ix);
      return newdcomplex(rxd - 1.0, ixd);
    }
  default: return NIL; /* to keep compiler happy */
  }
}

LOCAL LVAL gcd2 P2C(LVAL, n, LVAL, m)           /* euclid's algorith */
{
  LVAL r;

  xlstkcheck(3);
  xlprotect(n);
  xlprotect(m);
  xlsave(r);
  for (;;) {
    idiv2(m, n, NULL, &r, 'I');
    if (zerop(r))
      break;
    m = n;
    n = r;
  }
  xlpopn(3);
  return (n);
}

LVAL xadd(V)
{
  LVAL x, y;

  xlsave1(x);
  x = moreargs() ? xlganumber() : FIXZERO;
  while (moreargs()) {
    y = xlgetarg();
    x = add2(x, y);
  }
  xlpop();
  return x;
}

LVAL xsub(V)
{
  LVAL x, y;

  x = xlgetarg();
  if (moreargs()) {
    xlprot1(x);
    do {
      y = xlgetarg();
      x = sub2(x, y);
    } while (moreargs());
    xlpop();
  }
  else
    x = uminus(x);
  return x;
}

LVAL xmul(V)
{
  LVAL x, y;

  xlsave1(x);
  x = moreargs() ? xlganumber() : FIXONE;
  while (moreargs()) {
    y = xlgetarg();
    x = mul2(x, y);
  }
  xlpop();
  return x;
}

LVAL xdiv(V)
{
  LVAL x, y;

  x = xlgetarg();
  if (moreargs()) {
    xlprot1(x);
    do {
      y = xlgetarg();
      x = div2(x, y);
    } while (moreargs());
    xlpop();
  }
  else
    x = div2(FIXONE, x);
  return x;
}

#ifdef XLISP_STAT
LVAL xfdiv(V)
{
  LVAL x, y;

  x = xlgetarg();
  if (moreargs()) {
    xlprot1(x);
    do {
      y = xlgetarg();
      x = fdiv2(x, y);
    } while (moreargs());
    xlpop();
  }
  else
    x = div2(FIXONE, x);
  return x;
}
#endif /* XLISP_STAT */

LVAL xmin(V)
{
  LVAL x, y;

  x = xlganumber();
  while (moreargs()) {
    y = xlgetarg();
    x = min2(x, y);
  }
  return x;
}

LVAL xmax(V)
{
  LVAL x, y;

  x = xlganumber();
  while (moreargs()) {
    y = xlgetarg();
    x = max2(x, y);
  }
  return x;
}

#define DEFCMPFUN(name,fun) \
LVAL name(V) { \
  LVAL x, y; \
  x = xlganumber(); \
  while (moreargs()) { \
    y = xlgetarg(); \
    if (fun(x, y)) x = y; \
    else return NIL; \
  } \
  return s_true; \
}

DEFCMPFUN(xlss, lss2)
DEFCMPFUN(xleq, leq2)
DEFCMPFUN(xequ, equ2)
DEFCMPFUN(xgeq, geq2)
DEFCMPFUN(xgtr, gtr2)

LVAL xneq(V)
{
  LVAL x;
  int i;
  x = xlganumber();
  while (moreargs()) {
    for (i = 0; i < xlargc; i++)
      if (equ2(x, xlargv[i]))
	return NIL;
    x = xlgetarg();
  }
  return s_true;
}

#define DEFUNARYFUN(name,fun) \
LVAL name(V) { \
  LVAL x = xlgetarg(); \
  xllastarg(); \
  return fun(x); \
}

DEFUNARYFUN(xadd1, add1)
DEFUNARYFUN(xsub1, sub1)

LVAL xrem(V)
{
  LVAL x, y, r;

  x = xlgetarg();
  y = xlgetarg();
  xllastarg();
  idiv2(x, y, NULL, &r, 'I');
  return r;
}

LVAL xmod(V)
{
  LVAL x, y, r;

  x = xlgetarg();
  y = xlgetarg();
  xllastarg();
  idiv2(x, y, NULL, &r, '_');
  return r;
}

#ifdef MULVALS
#define DEFIDIVFUN(name,w) \
LVAL name (V) { \
  LVAL x, y, n, r; \
  x = xlgetarg(); \
  y = moreargs() ? xlgetarg() : FIXONE; \
  xllastarg(); \
  idiv2(x, y, &n, &r, w); \
  xlresults[0] = n; \
  xlresults[1] = r; \
  xlnumresults = 2; \
  return n; \
}
#define DEFFDIVFUN(name,w) \
LVAL name(V) { \
  FLOTYPE xd, yd, nd, rd; \
  xd = makefloat(xlgetarg()); \
  yd = moreargs() ? makefloat(xlgetarg()) : 1.0; \
  xllastarg(); \
  flonum_idiv2(xd, yd, &nd, &rd, w); \
  xlnumresults = 0; \
  xlresults[xlnumresults++] = cvflonum(nd); \
  xlresults[xlnumresults++] = cvflonum(rd); \
  return xlresults[0]; \
}
#else
#define DEFIDIVFUN(name,w) \
LVAL name(V) { \
  LVAL x, y, n, r; \
  x = xlgetarg(); \
  y = moreargs() ? xlgetarg() : FIXONE; \
  xllastarg(); \
  idiv2(x, y, &n, &r, w); \
  return n; \
}
#define DEFFDIVFUN(name,w) \
LVAL name (V) { \
  FLOTYPE xd, yd, nd, rd; \
  xd = makefloat(xlgetarg()); \
  yd = moreargs() ? makefloat(xlgetarg()) : 1.0; \
  xllastarg(); \
  flonum_idiv2(xd, yd, &nd, &rd, w); \
  return cvflonum(nd); \
}
#endif /* MULVALS */

DEFIDIVFUN(xfix, 'I')
DEFIDIVFUN(xceil, '^')
DEFIDIVFUN(xfloor, '_')
DEFIDIVFUN(xround, 'r')

DEFFDIVFUN(xffix, 'I')
DEFFDIVFUN(xfceil, '^')
DEFFDIVFUN(xffloor, '_')
DEFFDIVFUN(xfround, 'r')

LVAL xexpt(V)
{
  LVAL base, power;

  base = xlganumber();
  power = xlganumber();
  xllastarg();

  if (fixzerop(power)) {
    switch (ntype(base)) {
    case FIXNUM:
    case BIGNUM:
    case RATIO:
      return FIXONE;
    case FLONUM:
      return cvflonum((FLOTYPE) 1.0);
    case COMPLEX:
      return floatp(getreal(base)) ? newdcomplex(1.0, 0.0) : FIXONE;
    }
  }
  else if (zerop(base) && (zerop(power) || minusp(power)))
    badzero();
  
  /* complex float base or complex power */
  if ((complexp(base) && floatp(getreal(base))) || complexp(power)) {
    dcomplex zb, zp, zv;
    makecomplex(&zb, base);
    makecomplex(&zp, power);
    z_expt(&zv, &zb, &zp);
    return(cvcomplex(zv));
  }
  /* real or complex rational base, integer power */
  else if (integerp(power)) {
    LVAL val, r;

    if (fixp(base) && fixp(power)) {
      double fb, fp, fv;
      fb = (double) getfixnum(base);
      fp = (double) getfixnum(power);
      fv = floor(pow(fb, fp > 0.0 ? fp : -fp) + 0.5);
      if (infixnumrange(fv)) {
	if (fp > 0.0)
	  return cvfixnum((FIXTYPE) fv);
	else
	  return cvratio(1, (FIXTYPE) fv);
      }
    }
    else if (floatp(base) && fixp(power)) {
      double fb, fp, fv;
      fb = (double) getflonum(base);
      fp = (double) getfixnum(power);
      /* test range in case FIXNUMS aren't all exactly represented */
      if (infixnumrange(fp)) {
	fv = pow(fb, fp);
	return cvflonum((FLOTYPE) fv);
      }
      /* else fall through */
    }

    if (zerop(base))
      return FIXZERO;

    xlstkcheck(3);
    xlprotect(base);
    xlprotect(power);
    xlsave(val);
    val = FIXONE;
    if (minusp(power)) {
      base = div2(FIXONE, base);
      power = uminus(power);
    }
    while (TRUE) {
      idiv2(power, FIXTWO, &power, &r, 'I');
      if (! zerop(r)) {
	val = mul2(val, base);
	if (zerop(power))
	  break;
      }
      base = mul2(base, base);
    }
    xlpopn(3);
    return val;
  }
  /* real power and float base or rational power and real base */
  else {
    double fb, fp;
    fb = makefloat(base);
    fp = makefloat(power);
    if (fb == 0.0 && fp > 0.0)
      return(cvflonum((FLOTYPE) 0.0));
    else if (fb < 0.0) {
      dcomplex zb, zp, zv;
      makecomplex(&zb, base);
      makecomplex(&zp, power);
      z_expt(&zv, &zb, &zp);
      return(cvcomplex(zv));
    }
    else {
      double fval;
      if (fp < 0.0) {
	fb = 1.0 / fb;
	fp = -fp;
      }
      fval = pow(fb, fp);
      return(cvflonum((FLOTYPE) fval));
    }
  }
}  

#ifdef XLISP_STAT
LVAL xfexpt(V)
{
  /* this is a hack but it should do */
  if (xlargc == 2 && integerp(xlargv[1]) && minusp(xlargv[1])) {
    LVAL b = xlargv[0];
    if (rationalp(b))
      xlargv[0] = cvflonum((FLOTYPE) makefloat(b));
    else if (complexp(b) && rationalp(getreal(b)))
      xlargv[0] = newdcomplex(makefloat(getreal(b)), makefloat(getimag(b)));
  }
  return xexpt();
}
#endif /* XLISP_STAT */

/* arc tangent -- Tom Almy */
LVAL xatan(V)
{
  LVAL lnum, ldenom;
  dcomplex cnum, cdenom, cval;

  lnum = xlgetarg();

  if (moreargs()) {
    ldenom = xlgetarg();
    xllastarg();

    switch (commontype(lnum, ldenom)) {
    case IN:
    case FL:
    case RT:
      return cvflonum((FLOTYPE)atan2(makefloat(lnum), makefloat(ldenom)));
    default: /* complex */
      makecomplex(&cnum, lnum);
      makecomplex(&cdenom, ldenom);
      z_atan2(&cval, &cnum, &cdenom);
      return (cvcomplex(cval));
    }
  }
  else {
    switch (ntype(lnum)) {
    case FIXNUM:
    case BIGNUM:
    case RATIO:
    case FLONUM:
      return cvflonum((FLOTYPE)atan(makefloat(lnum)));
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
  LVAL m, n;

  if (!moreargs())		/* check for identity case */
    return FIXZERO;
  xlstkcheck(2);
  xlsave(m);
  xlsave(n);
  n = xlgainteger();
  if (minusp(n))
    n = uminus(n);	/* absolute value */
  checkzero(n);
  while (moreargs()) {
    m = xlgainteger();
    if (minusp(m))
      m = uminus(m);	/* absolute value */
    checkzero(m);
    n = gcd2(n,m);
  }
  xlpopn(2);
  return n;
}

LVAL xlcm(V)
{
  LVAL n, m, t, g;

  xlstkcheck(4);
  xlsave(n);
  xlsave(m);
  xlsave(t);
  xlsave(g);
  n = xlgainteger();
  while (moreargs())  {
    m = xlgainteger();
    if (zerop(n) || zerop(m))
      return FIXZERO;
    t = mul2(n, m);
    g = gcd2(n, m);
    n = div2(t, g);
  }
  if (minusp(n))
    n = uminus(n);
  xlpopn(4);

  return n;
}

LVAL xabs(V)
{
  LVAL x;

  x = xlgetarg();
  xllastarg();

  switch (ntype(x)) {
  case FIXNUM:
  case BIGNUM:
  case RATIO:
  case FLONUM:
    return minusp(x) ? uminus(x) : x;
  case COMPLEX:
    {
      dcomplex zx;
      makecomplex(&zx, x);
      return cvflonum((FLOTYPE) z_abs(&zx));
    }
  default:
    return xlbadtype(x);
  }
}

LVAL xsin(V)
{
 LVAL x;

 x = xlgetarg();
 xllastarg();

 switch (ntype(x)) {
 case FIXNUM:
 case BIGNUM:
 case RATIO:
 case FLONUM:
   return cvflonum((FLOTYPE) sin(makefloat(x)));
 case COMPLEX:
   {
     dcomplex zx;
     makecomplex(&zx, x);
     z_sin(&zx, &zx);
     return cvcomplex(zx);
   }
 default:
   return xlbadtype(x);
 }
}

LVAL xcos(V)
{
  LVAL x;

  x = xlgetarg();
  xllastarg();

  switch (ntype(x)) {
  case FIXNUM:
  case BIGNUM:
  case RATIO:
  case FLONUM:
    return cvflonum((FLOTYPE) cos(makefloat(x)));
  case COMPLEX:
    {
      dcomplex zx;
      makecomplex(&zx, x);
      z_cos(&zx, &zx);
      return cvcomplex(zx);
    }
  default:
    return xlbadtype(x);
  }
}

LVAL xtan(V)
{
  LVAL x;

  x = xlgetarg();
  xllastarg();

  switch (ntype(x)) {
  case FIXNUM:
  case BIGNUM:
  case RATIO:
  case FLONUM:
    return cvflonum((FLOTYPE) tan(makefloat(x)));
  case COMPLEX:
    {
      dcomplex zx;
      makecomplex(&zx, x);
      z_tan(&zx, &zx);
      return cvcomplex(zx);
    }
  default:
    return xlbadtype(x);
  }
}

LVAL xexp(V)
{
  LVAL x;

  x = xlgetarg();
  xllastarg();

  switch (ntype(x)) {
  case FIXNUM:
  case BIGNUM:
  case RATIO:
  case FLONUM:
    return cvflonum((FLOTYPE) exp(makefloat(x)));
  case COMPLEX:
    {
      dcomplex zx;
      makecomplex(&zx, x);
      z_exp(&zx, &zx);
      return cvcomplex(zx);
    }
  default:
    return xlbadtype(x);
  }
}

LVAL xsqrt(V)
{
  LVAL x;

  x = xlgetarg();
  xllastarg();

  switch (ntype(x)) {
  case FIXNUM:
  case BIGNUM:
  case RATIO:
  case FLONUM:
    {
      double dx = makefloat(x);
      if (dx >= 0.0)
	return cvflonum((FLOTYPE) sqrt(dx));
      else
	return newdcomplex(0.0, sqrt(-dx));
    }
  case COMPLEX:
    {
      dcomplex zx;
      makecomplex(&zx, x);
      z_sqrt(&zx, &zx);
      return cvcomplex(zx);
    }
  default:
    return xlbadtype(x);
  }
}

LVAL xfloat(V)
{
  LVAL x;

  x = xlgetarg();
  if (moreargs()) xlgaflonum();
  xllastarg();

  return floatp(x) ? x : cvflonum((FLOTYPE) makefloat(x));
}

LVAL xasin(V)
{
  LVAL x;

  x = xlgetarg();
  xllastarg();

  switch (ntype(x)) {
  case FIXNUM:
  case BIGNUM:
  case RATIO:
  case FLONUM:
    {
      double dx = makefloat(x);
      if (dx <= 1.0 && dx >= -1.0)
	return cvflonum((FLOTYPE) asin(dx));
      /* else drop through */
    }
  case COMPLEX:
    {
      dcomplex zx;
      makecomplex(&zx, x);
      z_asin(&zx, &zx);
      return cvcomplex(zx);
    }
  default:
    return xlbadtype(x);
  }
}

LVAL xacos(V)
{
  LVAL x;

  x = xlgetarg();
  xllastarg();

  switch (ntype(x)) {
  case FIXNUM:
  case BIGNUM:
  case RATIO:
  case FLONUM:
    {
      double dx = makefloat(x);
      if (dx <= 1.0 && dx >= -1.0)
	return cvflonum((FLOTYPE) acos(dx));
      /* else drop through */
    }
  case COMPLEX:
    {
      dcomplex zx;
      makecomplex(&zx, x);
      z_acos(&zx, &zx);
      return cvcomplex(zx);
    }
  default:
    return xlbadtype(x);
  }
}

LVAL xphase(V)
{
  LVAL x;

  x = xlgetarg();
  xllastarg();

  switch (ntype(x)) {
  case FIXNUM:
  case BIGNUM:
  case RATIO:
  case FLONUM:
    return cvflonum((FLOTYPE) minusp(x) ? PI : 0.0);
  case COMPLEX:
    {
      dcomplex zx;
      makecomplex(&zx, x);
      return cvflonum((FLOTYPE) z_phase(&zx));
    }
  default:
    return xlbadtype(x);
  }
}

LVAL xrational(V)
{
  LVAL x;

  x = xlgetarg();
  xllastarg();

  switch (ntype(x)) {
  case FIXNUM:
  case BIGNUM:
  case RATIO:
    return x;
  case FLONUM:
    {
      double dummy, original, dx;
      int e, sign;

      dx = original = getflonum(x);
      if (dx < 0.0) {
	sign = TRUE;
	dx = -dx;
      }
      else
	sign = FALSE;

      dx = frexp(dx, &e);
      while (MODF(dx, &dummy) != 0.0 && dx <= DBL_MAX / 2.0) {
	e--;
	dx *= 2.0;
      }
      if (e >= 0)
	return cvtflobignum(original);
      else {
	LVAL num, denom, val;
	xlstkcheck(2);
	xlsave(num);
	xlsave(denom);
	num = cvtflobignum(sign ? -dummy : dummy);
	denom = cvtflobignum(ldexp(1.0, -e));
	val = cvbratio(num, denom);
	xlpopn(2);
	return val;
      }
    }
  default:
    return xlbadtype(x);
  }
}

/* unary predicates */
#define DEFPREDFUN(name,fun) \
LVAL name(V) { \
  LVAL x; \
  x = xlganumber(); \
  xllastarg(); \
  return fun(x) ? s_true : NIL; \
}

DEFPREDFUN(xminusp, minusp)
DEFPREDFUN(xzerop, zerop)
DEFPREDFUN(xplusp, plusp)
DEFPREDFUN(xevenp, evenp)
DEFPREDFUN(xoddp, oddp)

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
    return newcomplex(real, imag);
  }
  if (rationalp(real)) return real;
  return newdcomplex(makefloat(real), 0.0);
}

LVAL xconjugate(V)
{
  LVAL arg, nimag, val;

  arg = xlgetarg();
  xllastarg();
  switch (ntype(arg)) {
  case FIXNUM:
  case BIGNUM:
  case RATIO:
  case FLONUM:
    return arg;
  case COMPLEX:
    xlsave1(nimag);
    nimag = uminus(getimag(arg));
    val = newcomplex(getreal(arg), nimag);
    xlpop();
    return val;
  default:
    return xlbadtype(arg);
  }
}

LVAL xrealpart(V)
{
  LVAL arg = xlgetarg();

  xllastarg();
  switch (ntype(arg)) {
  case FIXNUM:
  case BIGNUM:
  case RATIO:
  case FLONUM:
    return arg;
  case COMPLEX:
    return getreal(arg);
  default:
    xlbadtype(arg);
    return(NIL);
  }
}

LVAL ximagpart(V)
{
  LVAL arg = xlgetarg();

  xllastarg();
  switch (ntype(arg)) {
  case FIXNUM:
  case BIGNUM:
  case RATIO:
    return FIXZERO;
  case FLONUM:
    return cvflonum((FLOTYPE) 0.0);
  case COMPLEX:
    return(getimag(arg));
  default:
    xlbadtype(arg);
    return(NIL);
  }
}
#endif
