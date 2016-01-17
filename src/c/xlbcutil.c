#include "xlisp.h"
#ifdef BYTECODE
#ifdef XLISP_STAT
#include "xlstat.h"
#endif

LVAL xladd1 P1C(LVAL, x)
{
  switch (ntype(x)) {
  case FIXNUM:
    {
      FIXTYPE ix = getfixnum(x);
      FIXTYPE iv = ix + 1;
      if (iv > 0 || ix < 0)
	return cvfixnum((FIXTYPE) iv);
      else
	break;
    }
  case FLONUM:
    return(cvflonum(getflonum(x) + 1.0));
  }
#ifdef XLISP_STAT
  return xlcallsubr1(xsradd1, x);
#else
  return xlcallsubr1(xadd1, x);
#endif
}

LVAL xlsub1 P1C(LVAL, x)
{
  switch(ntype(x)) {
  case FIXNUM:
    {
      FIXTYPE ix = getfixnum(x);
      FIXTYPE iv = ix - 1;
      if (ix > 0 || iv < 0)
	return cvfixnum((FIXTYPE) iv);
      else
	break;
    }
  case FLONUM:
    return(cvflonum(getflonum(x) - 1.0));
  }
#ifdef XLISP_STAT
  return xlcallsubr1(xsrsub1, x);
#else
  return xlcallsubr1(xsub1, x);
#endif
}

#ifndef MAXFLOFIX
#define MAXFLOFIX  2147483647.0
#endif
#ifndef MINFLOFIX
#define MINFLOFIX -2147483648.0
#endif

#define goodisum(x, y, z) (((x) > 0) ? ((y) < (z)) : ! ((y) < (z)))
#define goodidiff(x, y, z) (!(((x < 0) ^ (y < 0)) && ((z < 0) ^ (x < 0))))
#define infixnumrange(x) (MINFLOFIX <= (x) && (x) <= MAXFLOFIX)

LVAL xladd2 P2C(LVAL, x, LVAL, y)
{
  switch (ntype(x)) {
  case FIXNUM:
    switch (ntype(y)) {
    case FIXNUM:
      {
	FIXTYPE ix = getfixnum(x);
	FIXTYPE iy = getfixnum(y);
	FIXTYPE iv = ix + iy;
	if (goodisum(ix,iy,iv))
	  return cvfixnum((FIXTYPE) iv);
	else
	  break;
      }
    case FLONUM: return cvflonum(getfixnum(x) + getflonum(y));
    }
    break;
  case FLONUM:
    switch (ntype(y)) {
    case FIXNUM: return cvflonum(getflonum(x) + getfixnum(y));
    case FLONUM: return cvflonum(getflonum(x) + getflonum(y));
    }
    break;
  }
#ifdef XLISP_STAT
  return(xlcallsubr2(xsradd, x, y));
#else
  return(xlcallsubr2(xadd, x, y));
#endif
}

LVAL xlsub2 P2C(LVAL, x, LVAL, y)
{
  switch (ntype(x)) {
  case FIXNUM:
    switch (ntype(y)) {
    case FIXNUM:
      {
	FIXTYPE ix = getfixnum(x);
	FIXTYPE iy = getfixnum(y);
	FIXTYPE iv = ix - iy;
	if (goodidiff(ix,iy,iv))
	  return cvfixnum((FIXTYPE) iv);
	else
	  break;
      }
    case FLONUM:
      return cvflonum(getfixnum(x) - getflonum(y));
    }
    break;
  case FLONUM:
    switch (ntype(y)) {
    case FIXNUM: return cvflonum(getflonum(x) - getfixnum(y));
    case FLONUM: return cvflonum(getflonum(x) - getflonum(y));
    }
    break;
  }
#ifdef XLISP_STAT
  return(xlcallsubr2(xsrsub, x, y));
#else
  return(xlcallsubr2(xsub, x, y));
#endif
}

LVAL xlmul2 P2C(LVAL, x, LVAL, y)
{
  switch (ntype(x)) {
  case FIXNUM:
    switch (ntype(y)) {
    case FIXNUM:
      {
	FIXTYPE ix = getfixnum(x);
	FIXTYPE iy = getfixnum(y);
	FLOTYPE rv = ((FLOTYPE) ix) * ((FLOTYPE) iy);
	if (infixnumrange(rv))
	  return cvfixnum((FIXTYPE) rv);
	else
	  break;
      }
    case FLONUM:
      return cvflonum(getfixnum(x) * getflonum(y));
    }
    break;
  case FLONUM:
    switch (ntype(y)) {
    case FIXNUM: return cvflonum(getflonum(x) * getfixnum(y));
    case FLONUM: return cvflonum(getflonum(x) * getflonum(y));
    }
    break;
  }
#ifdef XLISP_STAT
  return(xlcallsubr2(xsrmul, x, y));
#else
  return(xlcallsubr2(xmul, x, y));
#endif
}

#ifdef XLISP_STAT
#ifdef BIGNUMS
LVAL xldiv2 P2C(LVAL, x, LVAL, y) { return(xlcallsubr2(xsrfdiv, x, y)); }
#else
LVAL xldiv2 P2C(LVAL, x, LVAL, y) { return(xlcallsubr2(xsrdiv, x, y)); }
#endif /* BIGNUMS */
#else
LVAL xldiv2 P2C(LVAL, x, LVAL, y) { return(xlcallsubr2(xdiv, x, y)); }
#endif

LVAL xlmin2 P2C(LVAL, x, LVAL, y)
{
  switch (ntype(x)) {
  case FIXNUM:
    switch (ntype(y)) {
    case FIXNUM: return (getfixnum(x) < getfixnum(y)) ? x : y;
    case FLONUM: return (getfixnum(x) < getflonum(y)) ? x : y;
    }
    break;
  case FLONUM:
    switch (ntype(y)) {
    case FIXNUM: return (getflonum(x) < getfixnum(y)) ? x : y;
    case FLONUM: return (getflonum(x) < getflonum(y)) ? x : y;
    }
    break;
  }
#ifdef XLISP_STAT
  return(xlcallsubr2(xsmin, x, y));
#else
  return(xlcallsubr2(xmin, x, y));
#endif
}

LVAL xlmax2 P2C(LVAL, x, LVAL, y)
{
  switch (ntype(x)) {
  case FIXNUM:
    switch (ntype(y)) {
    case FIXNUM: return (getfixnum(x) > getfixnum(y)) ? x : y;
    case FLONUM: return (getfixnum(x) > getflonum(y)) ? x : y;
    }
    break;
  case FLONUM:
    switch (ntype(y)) {
    case FIXNUM: return (getflonum(x) > getfixnum(y)) ? x : y;
    case FLONUM: return (getflonum(x) > getflonum(y)) ? x : y;
    }
    break;
  }
#ifdef XLISP_STAT
  return(xlcallsubr2(xsmax, x, y));
#else
  return(xlcallsubr2(xmax, x, y));
#endif
}

#define DEFTEST(name,op,fun) \
LVAL name P2C(LVAL, x, LVAL, y) \
{ \
  switch (ntype(x)) { \
  case FIXNUM: \
    switch (ntype(y)) { \
    case FIXNUM: return (getfixnum(x) op getfixnum(y)) ? s_true : NIL; \
    case FLONUM: return (getfixnum(x) op getflonum(y)) ? s_true : NIL; \
    } \
    break; \
  case FLONUM: \
    switch (ntype(y)) { \
    case FIXNUM: return (getflonum(x) op getfixnum(y)) ? s_true : NIL; \
    case FLONUM: return (getflonum(x) op getflonum(y)) ? s_true : NIL; \
    } \
    break; \
  } \
  return(xlcallsubr2(fun, x, y)); \
}

#ifdef XLISP_STAT
DEFTEST(xllss2,< ,xsrlss)
DEFTEST(xlleq2,<=,xsrleq)
DEFTEST(xlequ2,==,xsrequ)
DEFTEST(xlneq2,!=,xsrneq)
DEFTEST(xlgeq2,>=,xsrgeq)
DEFTEST(xlgtr2,> ,xsrgtr)
#else
DEFTEST(xllss2,< ,xlss)
DEFTEST(xlleq2,<=,xleq)
DEFTEST(xlequ2,==,xequ)
DEFTEST(xlneq2,!=,xneq)
DEFTEST(xlgeq2,>=,xgeq)
DEFTEST(xlgtr2,> ,xgtr)
#endif

int num_cmp2 P3C(int, which, LVAL, x, LVAL, y)
{
  switch (ntype(x)) {
  case FIXNUM:
    switch (ntype(y)) {
    case FIXNUM:
      {
	FIXTYPE ix = getfixnum(x);
	FIXTYPE iy = getfixnum(y);
	switch (which) {
	case '<': return ix <  iy ? TRUE : FALSE;
	case 'L': return ix <= iy ? TRUE : FALSE;
	case '=': return ix == iy ? TRUE : FALSE;
	case '#': return ix != iy ? TRUE : FALSE;
	case 'G': return ix >= iy ? TRUE : FALSE;
	case '>': return ix >  iy ? TRUE : FALSE;
	}
      }
      break;
    case FLONUM:
      {
	FIXTYPE ix = getfixnum(x);
	FLOTYPE ry = getflonum(y);
	switch (which) {
	case '<': return ix <  ry ? TRUE : FALSE;
	case 'L': return ix <= ry ? TRUE : FALSE;
	case '=': return ix == ry ? TRUE : FALSE;
	case '#': return ix != ry ? TRUE : FALSE;
	case 'G': return ix >= ry ? TRUE : FALSE;
	case '>': return ix >  ry ? TRUE : FALSE;
	}
      }
      break;
    }
    break;
  case FLONUM:
    switch (ntype(y)) {
    case FIXNUM:
      {
	FLOTYPE rx = getflonum(x);
	FIXTYPE iy = getfixnum(y);
	switch (which) {
	case '<': return rx <  iy ? TRUE : FALSE;
	case 'L': return rx <= iy ? TRUE : FALSE;
	case '=': return rx == iy ? TRUE : FALSE;
	case '#': return rx != iy ? TRUE : FALSE;
	case 'G': return rx >= iy ? TRUE : FALSE;
	case '>': return rx >  iy ? TRUE : FALSE;
	}
      }
      break;
    case FLONUM:
      {
	FLOTYPE rx = getflonum(x);
	FLOTYPE ry = getflonum(y);
	switch (which) {
	case '<': return rx <  ry ? TRUE : FALSE;
	case 'L': return rx <= ry ? TRUE : FALSE;
	case '=': return rx == ry ? TRUE : FALSE;
	case '#': return rx != ry ? TRUE : FALSE;
	case 'G': return rx >= ry ? TRUE : FALSE;
	case '>': return rx >  ry ? TRUE : FALSE;
	}
      }
      break;
    }
    break;
  }
  switch (which) {
  case '<': return(!null(xllss2(x, y)));
  case 'L': return(!null(xlleq2(x, y)));
  case '=': return(!null(xlequ2(x, y)));
  case '#': return(!null(xlneq2(x, y)));
  case 'G': return(!null(xlgeq2(x, y)));
  case '>': return(!null(xlgtr2(x, y)));
  default:  return(FALSE);
  }
}

#ifndef XLISP_STAT
LVAL slot_value P2C(LVAL, x, LVAL, y)
{
  xlfail("slot value not available");
  return(NIL);
}

LVAL set_slot_value(x, y, z)
     LVAL x, y, z;
{
  xlfail("slot value not available");
  return(NIL);
}
#endif
#endif /* BYTECODE */
